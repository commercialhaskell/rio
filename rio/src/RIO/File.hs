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
  , ensureFileDurable
  )
  where

import           RIO.Prelude.Reexports

#ifdef WINDOWS
import           RIO.Prelude.IO

#else

import           RIO.Directory          (doesFileExist)
import           RIO.ByteString         (hPut)
import           Data.Bits              ((.|.))
import           Data.Typeable          (cast)
import           Foreign.C              (CInt (..), throwErrnoIfMinus1,
                                         throwErrnoIfMinus1Retry)
import           GHC.IO.Device          (IODeviceType (RegularFile))
import qualified GHC.IO.Device          as Device
import qualified GHC.IO.FD              as FD
import qualified GHC.IO.Handle.FD       as HandleFD
import           System.Directory       (copyFile)
import           System.FilePath        (takeDirectory, takeFileName, (</>))
import           System.Posix.Internals (CFilePath, c_close, c_safe_open,
                                         withFilePath)
import           System.Posix.Types     (CMode (..), Fd (..))

#if MIN_VERSION_base(4,9,0)
import qualified GHC.IO.Handle.Types    as HandleFD (Handle (..), Handle__ (..))
#endif


-- TODO: Add a ticket/pull request to export this symbols from
-- System.Internal.Posix
--
-- NOTE: System.Posix.Internal doesn't re-export this constants so we have to
-- recreate-them here
foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_wronly" o_WRONLY :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"   o_RDWR   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_append" o_APPEND :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_noctty" o_NOCTTY :: CInt

-- After here, we have our own imports
foreign import ccall safe "fcntl.h openat"
  c_safe_openat :: CInt -> CFilePath -> CInt -> CMode -> IO CInt

foreign import ccall safe "fcntl.h renameat"
  c_safe_renameat :: CInt -> CFilePath -> CInt -> CFilePath -> IO CInt

foreign import ccall safe "unistd.h fsync"
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
    ReadMode      -> read_flags
    WriteMode     -> write_flags
    ReadWriteMode -> rw_flags
    AppendMode    -> append_flags

openDir :: MonadIO m => FilePath -> m Fd
openDir fp
  -- TODO: Investigate what is the situation with Windows FS in regards to non_blocking
  -- NOTE: File operations _do not support_ non_blocking on various kernels, more
  -- info can be found here: https://ghc.haskell.org/trac/ghc/ticket/15153
 =
  liftIO $
  withFilePath fp $ \cFp ->
    Fd <$>
    (throwErrnoIfMinus1Retry "openDir" $
     c_safe_open cFp (ioModeToFlags ReadMode) 0o660)

-- NOTE: Not Async exception safe
openFileFromDir :: (MonadIO m) => Fd -> FilePath -> IOMode -> m Handle
openFileFromDir (Fd dirFd) fp iomode =
  liftIO $
  withFilePath fp $ \f -> do
    fileFd <-
      throwErrnoIfMinus1Retry "openFileFromDir" $
      c_safe_openat dirFd f (ioModeToFlags iomode) 0o666
    (fD, fd_type) <-
      FD.mkFD
        fileFd
        iomode
        Nothing {- no stat -}
        False {- not a socket -}
        False {- non_blocking --}
         -- TODO: We want to investigate if we can do non-blocking here
       `catchAny` \e -> do
        void (c_close fileFd)
        throwIO e
    -- we want to truncate() if this is an open in WriteMode, but only if the
    -- target is a RegularFile. ftruncate() fails on special files like
    -- /dev/null.
    when (iomode == WriteMode && fd_type == RegularFile) $ Device.setSize fD 0
    HandleFD.mkHandleFromFD fD fd_type fp iomode False Nothing `onException`
      (liftIO $ Device.close fD)

-- | Opens a file using the openat C low-level API. This approach allows us to
-- get a file descriptor for the directory that contains the file, which we can
-- use later on to fsync the directory with.
--
-- @since 0.1.6
openFileDurable :: MonadIO m => FilePath -> IOMode -> m (Fd, Handle)
openFileDurable absFp iomode =  do
  let dir = takeDirectory absFp
      fp = takeFileName absFp

  dirFd <- openDir dir
  fileHandle <- openFileFromDir dirFd fp iomode
  return (dirFd, fileHandle)

-- | This sub-routine does the following tasks:
--
-- * It calls fsync and then closes the given handler (mapping to a temporal/backup filepath)
-- * It calls fsync and then closes the containing directory of the file
--
-- These steps guarante that the file changes are durable.
--
-- @since 0.1.6
closeFileDurable :: MonadIO m => Fd -> Handle -> m ()
closeFileDurable (Fd dirFd) h =
  liftIO $
  finally
    (do fileFd <- handleToFd h
        fsyncDescriptor "File" (FD.fdFD fileFd) `finally` hClose h
        fsyncDescriptor "Directory" dirFd)
    closeDirectory
  where
    fsyncDescriptor name fd =
      void $
      throwErrnoIfMinus1 ("closeFileDurable - fsync(" <> name <> ")") $
      c_safe_fsync fd
    closeDirectory =
      void $
      throwErrnoIfMinus1Retry "closeFileDurable - close(Directory)" $
      c_close dirFd

toTmpFilePath :: FilePath -> FilePath
toTmpFilePath filePath =
    dirPath </> tmpFilename
  where
    dirPath = takeDirectory filePath
    filename = takeFileName filePath
    tmpFilename = "." <> filename <> ".tmp"



handleToFd :: Handle -> IO FD.FD
#if MIN_VERSION_base(4,9,0)
handleToFd h =
  case h of
    HandleFD.FileHandle _ mv -> do
      HandleFD.Handle__{HandleFD.haDevice = dev} <- readMVar mv
      case cast dev of
        Just fd -> return fd
        Nothing -> error "not a file handle"
    HandleFD.DuplexHandle {} -> error "not a file handle"
# else
handleToFd = HandleFD.handleToFd
#endif

-- | This sub-routine does the following tasks:
--
-- * It calls fsync and then closes the given handler (mapping to a temporal/backup filepath)
-- * It renames the file to the original path (using renameat)
-- * It calls fsync and then closes the containing directory of the file
--
-- These steps guarante that the file are durable, and that the backup mechanism
-- for catastrophic failure is discarded after no error is thrown.
--
-- @since 0.1.6
closeFileDurableAtomic ::
     MonadIO m => FilePath -> Fd -> Handle -> m ()
closeFileDurableAtomic filePath (Fd dirFd) fileHandler =
  liftIO $
  finally
    (withFilePath tmpFilePath $ \tmpFp ->
       withFilePath filePath $ \fp -> do
         fileFd <- handleToFd fileHandler
         fsyncDescriptor "File" (FD.fdFD fileFd)
           `finally` hClose fileHandler
         renameFile tmpFp fp
         fsyncDescriptor "Directory" dirFd)
    closeDirectory
  where
    tmpFilePath = toTmpFilePath filePath
    fsyncDescriptor name fd =
      void $
      throwErrnoIfMinus1 ("closeFileDurableAtomic - fsync(" <> name <> ")") $
      c_safe_fsync fd
    renameFile tmpFp origFp =
      void $
      throwErrnoIfMinus1Retry "closeFileDurableAtomic - renameFile" $
      c_safe_renameat dirFd tmpFp dirFd origFp
    closeDirectory =
      void $
      throwErrnoIfMinus1Retry "closeFileDurableAtomic - close(Directory)" $
      c_close dirFd

#endif

-- | After a file is closed, it opens it again and executes fsync internally on
-- both the file and the directory that contains it.
--
-- [The effectiveness of calling this function is
-- debatable](https://stackoverflow.com/questions/37288453/calling-fsync2-after-close2/50158433#50158433),
-- as this relies on internal implementation details at the Kernel level that
-- might change. We argue that, despite this fact, calling this function may
-- bring benefits in terms of durability
--
-- @since 0.1.6
ensureFileDurable :: MonadUnliftIO m => FilePath -> m ()
ensureFileDurable absFp =
#if WINDOWS
  absFp `seq` return ()
#else
  bracket (openFileDurable absFp ReadMode)
          (uncurry closeFileDurable)
          (const $ return ())
#endif


-- | Similar to 'writeFileBinary', but it also ensures that changes executed to
-- the file are guaranteed to be durable. It internally uses fsync and makes
-- sure it synchronizes the file on disk.
--
-- @since 0.1.6
writeFileBinaryDurable :: MonadUnliftIO m => FilePath -> ByteString -> m ()
writeFileBinaryDurable absFp bytes =
#if WINDOWS
  writeFileBinary absFp bytes
#else
  withFileDurable absFp WriteMode (liftIO . (`hPut` bytes))
#endif

-- | Similar to 'writeFileBinary', but it also guarantes that changes executed
-- to the file are durable, also, in case of failure, the modified file is never
-- going to get corrupted. It internally uses fsync and makes sure it
-- synchronizes the file on disk.
--
-- @since 0.1.6
writeFileBinaryDurableAtomic :: MonadUnliftIO m => FilePath -> ByteString -> m ()
writeFileBinaryDurableAtomic fp bytes =
#if WINDOWS
  writeFileBinary fp bytes
#else
  withFileDurableAtomic fp WriteMode (liftIO . (`hPut` bytes))
#endif

-- | Opens a file with the following guarantees:
--
-- * Successfully closes the file in case of an asynchronous exception
--
-- * It reliably saves the file in the correct directory; including edge case
--   situations like a different device being mounted to the current directory,
--   or the current directory being renamed to some other name while the file is
--   being used.
--
-- * It ensures durability by executing fsync before close
--
-- @since 0.1.6
withFileDurable ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFileDurable absFp iomode cb =
#if WINDOWS
  withFile absFp iomode cb
#else
  withRunInIO $ \run ->
    bracket
      (openFileDurable absFp iomode)
      (uncurry closeFileDurable)
      (\(_, h) -> do run (cb h))
#endif

-- | Opens a file with the following guarantees:
--
-- * Successfully closes the file in case of an asynchronous exception
--
-- * It reliably saves the file in the correct directory; including edge case
--   situations like a different device being mounted to the current directory,
--   or the current directory being renamed to some other name while the file is
--   being used.
--
-- * It ensures durability by executing fsync before close
--
-- * It keeps all changes in a copy file, and after is closed it renames it to
--   the original filepath, in case of catastrophic failure, the original file
--   stays unaffected.
--
--
-- === Performance Considerations
--
-- When using an 'IOMode' of 'WriteMode', 'ReadWriteMode', or 'AppendMode', this function
-- performs a copy operation of the file being read. This may be prohibitive in
-- scenarios where the input file is expected to be large.
--
-- @since 0.1.6
withFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withFileDurableAtomic absFp iomode cb = do
#if WINDOWS
  withFile absFp iomode cb
#else
  withRunInIO $ \run ->
    case iomode of
        -- We need to consider an atomic operation only when we are on 'WriteMode'
      ReadMode -> run (withFile absFp iomode cb)
        -- We use regular old withFile operation
      _ {- WriteMode, ReadWriteMode,  AppendMode -}
       -> do
        -- copy original file for read purposes
        fileExists <- doesFileExist absFp
        when fileExists $
          copyFile absFp (toTmpFilePath absFp)

        withDurableAtomic run
  where
    withDurableAtomic run =
      bracket
        (openFileDurable (toTmpFilePath absFp) iomode)
        (uncurry $ closeFileDurableAtomic absFp)
        (\(_, h) -> run (cb h))
#endif
