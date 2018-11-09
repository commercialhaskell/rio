{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
module RIO.File
  (
    writeFileBinaryDurable
  , writeFileBinaryDurableAtomic
  , withFileDurable
  , withFileDurableAtomic
  )
  where

import           RIO.ByteString         (hPut)
import           RIO.Prelude.Reexports
import           RIO.Prelude.Trace

import           Data.Bits              ((.|.))
import           Foreign.C              (CInt (..), throwErrnoIfMinus1Retry)
import           GHC.IO.Device          (IODeviceType (RegularFile))
import qualified GHC.IO.Device          as Device
import qualified GHC.IO.FD              as FD
import           GHC.IO.Handle.FD       (mkHandleFromFD, fdToHandle, handleToFd)
import           System.FilePath        (takeDirectory, takeFileName)
import           System.Posix.Internals (CFilePath, c_safe_open, c_close, withFilePath)
import           System.Posix.Types     (CMode (..))

-- NOTE: System.Posix.Internal doesn't re-export this constants
-- so we have to recreate-them
foreign import ccall unsafe "RIOFile.h __rio_o_rdonly" o_RDONLY :: CInt
foreign import ccall unsafe "RIOFile.h __rio_o_wronly" o_WRONLY :: CInt
foreign import ccall unsafe "RIOFile.h __rio_o_rdwr"   o_RDWR   :: CInt
foreign import ccall unsafe "RIOFile.h __rio_o_append" o_APPEND :: CInt
foreign import ccall unsafe "RIOFile.h __rio_o_creat"  o_CREAT  :: CInt
foreign import ccall unsafe "RIOFile.h __rio_o_noctty" o_NOCTTY :: CInt
foreign import ccall safe "RIOFile.h __rio_openat"
  c_safe_openat :: CInt -> CFilePath -> CInt -> CMode -> IO CInt
foreign import ccall safe "RIOFile.h __rio_renameat"
  c_safe_renameat :: CInt -> CFilePath -> CInt -> CFilePath -> IO CInt
foreign import ccall safe "RIOFile.h __rio_fsync"
  c_safe_fsync :: CInt -> IO CInt

std_flags, output_flags, read_flags, write_flags, rw_flags,
    append_flags :: CInt
std_flags    = o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND

ioModeToFlags :: IOMode -> CInt
ioModeToFlags iomode =
  case iomode of
    ReadMode -> read_flags
    WriteMode -> write_flags
    ReadWriteMode -> rw_flags
    AppendMode -> append_flags

openDirFD :: MonadIO m => FilePath -> m CInt
openDirFD fp
  -- TODO: Investigate what is the situation with Windows FS in regards to non_blocking
  -- NOTE: File operations _do not support_ non_blocking on various kernels, more
  -- info can be found here: https://ghc.haskell.org/trac/ghc/ticket/15153
 =
  liftIO $
  withFilePath fp $ \cFp -> c_safe_open cFp (ioModeToFlags ReadMode) 0o660


openFileFromDir :: (MonadIO m) => CInt -> FilePath -> IOMode -> m Handle
openFileFromDir dirFd fp iomode =
  liftIO $ do
    withFilePath fp $ \f ->
      let oflags1 = ioModeToFlags iomode
      -- TODO: Add support for mingw32
          binary_flags = 0
          oflags = oflags1 .|. binary_flags
       in do fd <-
               throwErrnoIfMinus1Retry "openFileFromDir" $
               c_safe_openat dirFd f oflags 0o666
             (fD, fd_type) <-
               FD.mkFD
                 fd
                 iomode
                 Nothing {- no stat -}
                 False {- not a socket -}
                 False {- non_blocking --}
                `catchAny` \e -> do
                 _ <- c_close fd
                 _ <- c_close dirFd
                 throwIO e
      -- we want to truncate() if this is an open in WriteMode, but only if the
      -- target is a RegularFile. ftruncate() fails on special files like
      -- /dev/null.
             when (iomode == WriteMode && fd_type == RegularFile) $
               Device.setSize fD 0
             -- result <- fdToHandle (FD.fdFD fD)
             mkHandleFromFD fD fd_type fp iomode False Nothing
               `onException` (liftIO $ Device.close fD)

closeFileFromDir :: MonadIO m => CInt -> Handle -> m ()
closeFileFromDir dirFdCInt h =
  liftIO $ do
    fileFd <- handleToFd h
    void $
      throwErrnoIfMinus1Retry "closeFileFromDir" $
      c_safe_fsync (FD.fdFD fileFd)
    hClose h
    void $
      throwErrnoIfMinus1Retry "closeFileFromDir" $
      c_safe_fsync dirFdCInt
    void $ throwErrnoIfMinus1Retry "closeFileFromDir" $ c_close dirFdCInt

-- 'Handle' maps to the swap file
closeSwapFileFromDir ::
     MonadIO m => FilePath -> FilePath -> CInt -> Handle -> m ()
closeSwapFileFromDir swp orig dirFdCInt h =
  liftIO $
  withFilePath swp $ \swpFp ->
    withFilePath orig $ \origFp -> do
      fileFd <- handleToFd h
      void $
        throwErrnoIfMinus1Retry "closeSwapFileFromDir" $
        c_safe_fsync (FD.fdFD fileFd)
      hClose h
      void $
        throwErrnoIfMinus1Retry "closeSwapFileFromDir" $
        c_safe_renameat dirFdCInt swpFp dirFdCInt origFp
      void $
        throwErrnoIfMinus1Retry "closeSwapFileFromDir" $
        c_safe_fsync dirFdCInt
      void $ throwErrnoIfMinus1Retry "closeFileFromDir" $ c_close dirFdCInt

-- |
-- @since 0.1.6
writeFileBinaryDurable :: MonadUnliftIO m => FilePath -> ByteString -> m ()
writeFileBinaryDurable absFp bytes =
  withFileDurable absFp WriteMode (liftIO . (`hPut` bytes))
  -- liftIO $ do
  --   let dir = takeDirectory absFp
  --       fp = takeFileName absFp
  --   bracket
  --     (do dirFd <- openDirFD dir WriteMode
  --         fileHandle <- openFileFromDir dirFd fp WriteMode
  --         return (dirFd, fileHandle))
  --     (uncurry closeFileFromDir)
  --     (\(_, h) -> hPut h bytes)

-- |
-- @since 0.1.6
writeFileBinaryDurableAtomic :: MonadUnliftIO m => FilePath -> ByteString -> m ()
writeFileBinaryDurableAtomic absFp bytes =
  withFileDurableAtomic absFp WriteMode (liftIO . (`hPut` bytes))
  -- liftIO $ do
  --   let dir = takeDirectory absFp
  --       origFp = takeFileName absFp
  --       swpFp = origFp <> ".swp"
  --   bracket
  --     (do dirFd <- openDirFD dir WriteMode
  --         fileHandle <- openFileFromDir dirFd swpFp WriteMode
  --         return (dirFd, fileHandle))
  --     (uncurry $ closeSwapFileFromDir swpFp origFp)
  --     (\(_, h) -> hPut h bytes)

-- |
-- @since 0.1.6
withFileDurable ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFileDurable absFp iomode cb =
  withRunInIO $ \run -> do
    let dir = takeDirectory absFp
        fp = takeFileName absFp
    bracket
      (do dirFd <- openDirFD dir
          fileHandle <- openFileFromDir dirFd fp iomode
          return (dirFd, fileHandle))
      (uncurry closeFileFromDir)
      (\(_, h) -> do
         run (cb h))

-- |
-- @since 0.1.6
withFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFileDurableAtomic absFp iomode cb =
  withRunInIO $ \run ->
    case iomode
    -- We need to consider an atomic operation only when we are on 'WriteMode'
          of
      WriteMode -> do
        let dir = takeDirectory absFp
            origFp = takeFileName absFp
            swpFp = origFp <> ".swp"
        bracket
          (do dirFd <- openDirFD dir
              fileHandle <- openFileFromDir dirFd swpFp WriteMode
              return (dirFd, fileHandle))
          (uncurry $ closeSwapFileFromDir swpFp origFp)
          (\(_, h) -> run (cb h))
      _ -> run (withFileDurable absFp iomode cb)
