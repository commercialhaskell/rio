{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-|

== Rationale

This module offers functions to handle files that offer better durability and/or
atomicity guarantees.

== When to use the functions on this module?

Given the usage of this functions comes at a cost in performance, it is important
to consider what are the use cases that are ideal for each of the functions.

=== Not Durable and not Atomic

For this use case, you want to use the regular functions:

* 'System.IO.withFile'
* 'RIO.writeFileBinary'

The regular use case for this scenario happens when your program is dealing with
outputs that are never going to be consumed again (e.g. a report that gets
generated from other data sources).

=== Atomic but not Durable

The regular use case for this scenario is when you have a multi-step algorithm
that builds temporal artifacts, you care about them being atomic, but your
program recreates them each time. (e.g. @ghc --make@ generating @.o@ files).

There is no function exported by this module that provides only atomicity.

=== Durable but not Atomic

For this use case, you want to use the functions:

* 'withBinaryFileDurable'
* 'writeBinaryFileDurable'

The regular use case for this scenario happens when your program deals with the
atomicity of your data through other means. (e.g. SQLite Write Ahead Log (WAL)
strategy).

=== Durable and Atomic

For this use case, you can use the functions:

* 'withBinaryFileDurableAtomic'
* 'writeBinaryFileDurableAtomic'

The regular use case for this scenario happens when you want to ensure that
after a program is executed, the data is there and it will be in a consistent
state, _always_.

@since 0.1.6
-}
module RIO.File
  (
    writeBinaryFileDurable
  , writeBinaryFileDurableAtomic
  , withBinaryFileDurable
  , withBinaryFileDurableAtomic
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
import           System.IO              (openBinaryTempFile)

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

closeDirectory :: MonadIO m => Fd -> m ()
closeDirectory (Fd dirFd) =
  liftIO $
  void $
  throwErrnoIfMinus1Retry "closeDirectory" $ c_close dirFd

fsyncFileDescriptor :: MonadIO m => String -> CInt -> m ()
fsyncFileDescriptor name cFd =
  liftIO $
  void $
    throwErrnoIfMinus1 ("fsync - " <> name) $
    c_safe_fsync cFd

-- NOTE: Not Async exception safe
openFileFromDir :: (MonadIO m) => Fd -> FilePath -> IOMode -> m Handle
openFileFromDir (Fd dirFd) fp iomode =
  liftIO $
  withFilePath fp $ \f -> do
    bracketOnError
      (do fileFd <- throwErrnoIfMinus1Retry "openFileFromDir" $
                      c_safe_openat dirFd f (ioModeToFlags iomode) 0o666
          FD.mkFD
             fileFd
             iomode
             Nothing {- no stat -}
             False {- not a socket -}
             False {- non_blocking -}
            `onException` c_close fileFd)
      (liftIO . Device.close . fst)
      (\(fD, fd_type) -> do
         -- we want to truncate() if this is an open in WriteMode, but only if the
         -- target is a RegularFile. ftruncate() fails on special files like
         -- /dev/null.
         when (iomode == WriteMode && fd_type == RegularFile) $
           Device.setSize fD 0
         HandleFD.mkHandleFromFD fD fd_type fp iomode False Nothing)

-- | Opens a file using the openat C low-level API. This approach allows us to
-- get a file descriptor for the directory that contains the file, which we can
-- use later on to fsync the directory with.
--
-- @since 0.1.6
openFileAndDirectory :: MonadUnliftIO m => FilePath -> IOMode -> m (Fd, Handle)
openFileAndDirectory absFp iomode =  do
  let dir = takeDirectory absFp
      fp = takeFileName absFp

  dirFd <- openDir dir
  fileHandle <- openFileFromDir dirFd fp iomode `onException` closeDirectory dirFd
  return (dirFd, fileHandle)

-- | This sub-routine does the following tasks:
--
-- * It calls fsync and then closes the given Handle (mapping to a temporal/backup filepath)
-- * It calls fsync and then closes the containing directory of the file
--
-- These steps guarante that the file changes are durable.
--
-- @since 0.1.6
closeFileDurable :: MonadIO m => Fd -> Handle -> m ()
closeFileDurable dirFd@(Fd cDirFd) h =
  liftIO $
  finally
    (do fileFd <- handleToFd h
        fsyncFileDescriptor "closeFileDurable/File" (FD.fdFD fileFd) `finally`
          hClose h
        -- NOTE: Here we are purposefully not fsyncing the directory if the file fails to fsync
        fsyncFileDescriptor "closeFileDurable/Directory" cDirFd)
    (closeDirectory dirFd)

buildTemporaryFilePath :: MonadUnliftIO m => FilePath -> m FilePath
buildTemporaryFilePath filePath = do
  let
    dirFp  = takeDirectory filePath
    fileFp = takeFileName filePath
  bracket (liftIO $ openBinaryTempFile dirFp fileFp)
          (hClose . snd)
          (return . fst)

toTmpFilePath :: MonadUnliftIO m => FilePath -> m FilePath
toTmpFilePath filePath =
    buildTemporaryFilePath (dirPath </> tmpFilename)
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
-- * It calls fsync and then closes the given Handle (mapping to a temporal/backup filepath)
-- * It renames the file to the original path (using renameat)
-- * It calls fsync and then closes the containing directory of the file
--
-- These steps guarantee that the file is durable, and that the backup mechanism
-- for catastrophic failure is discarded after no error is thrown.
--
-- @since 0.1.6
closeFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> FilePath -> Fd -> Handle -> m ()
closeFileDurableAtomic tmpFilePath filePath dirFd@(Fd cDirFd) fileHandle = do
  liftIO $
    finally
      (withFilePath tmpFilePath $ \tmpFp ->
         withFilePath filePath $ \fp -> do
           fileFd <- handleToFd fileHandle
           fsyncFileDescriptor "closeFileDurableAtomic/File" (FD.fdFD fileFd) `finally` hClose fileHandle
           renameFile tmpFp fp
           fsyncFileDescriptor "closeFileDurableAtomic/Directory" cDirFd)
      (closeDirectory dirFd)
  where
    renameFile tmpFp origFp =
      void $
      throwErrnoIfMinus1Retry "closeFileDurableAtomic - renameFile" $
      c_safe_renameat cDirFd tmpFp cDirFd origFp

#endif

-- | After a file is closed, it opens it again and executes fsync internally on
-- both the file and the directory that contains it. Note this function is
-- intended to work around the non-durability of existing file APIs, as opposed
-- to being necessary for the API functions provided in 'RIO.File' module.
--
-- [The effectiveness of calling this function is
-- debatable](https://stackoverflow.com/questions/37288453/calling-fsync2-after-close2/50158433#50158433),
-- as it relies on internal implementation details at the Kernel level that
-- might change. We argue that, despite this fact, calling this function may
-- bring benefits in terms of durability.
--
-- === Cross-Platform support
--
-- This function is a noop on Windows platforms.
--
-- @since 0.1.6
ensureFileDurable :: MonadUnliftIO m => FilePath -> m ()
ensureFileDurable absFp =
#if WINDOWS
  absFp `seq` return ()
#else
  bracket (openFileAndDirectory absFp ReadMode)
          (uncurry closeFileDurable)
          (const $ return ())
#endif


-- | Similar to 'writeFileBinary', but it also ensures that changes executed to
-- the file are guaranteed to be durable. It internally uses fsync and makes
-- sure it synchronizes the file on disk.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'RIO.writeFileBinary' on Windows platforms.
--
-- @since 0.1.6
writeBinaryFileDurable :: MonadUnliftIO m => FilePath -> ByteString -> m ()
writeBinaryFileDurable absFp bytes =
#if WINDOWS
  writeFileBinary absFp bytes
#else
  withBinaryFileDurable absFp WriteMode (liftIO . (`hPut` bytes))
#endif

-- | Similar to 'writeFileBinary', but it also guarantes that changes executed
-- to the file are durable, also, in case of failure, the modified file is never
-- going to get corrupted. It internally uses fsync and makes sure it
-- synchronizes the file on disk.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'RIO.writeFileBinary' on Windows platforms.
--
-- @since 0.1.6
writeBinaryFileDurableAtomic :: MonadUnliftIO m => FilePath -> ByteString -> m ()
writeBinaryFileDurableAtomic fp bytes =
#if WINDOWS
  writeFileBinary fp bytes
#else
  withBinaryFileDurableAtomic fp WriteMode (liftIO . (`hPut` bytes))
#endif

-- | Opens a file with the following guarantees:
--
-- * It successfully closes the file in case of an asynchronous exception
--
-- * It reliably saves the file in the correct directory; including edge case
--   situations like a different device being mounted to the current directory,
--   or the current directory being renamed to some other name while the file is
--   being used.
--
-- * It ensures durability by executing an fsync call before closing the file
--   handle
--
-- === Cross-Platform support
--
-- This function behaves the same as 'System.IO.withFile' on Windows platforms.
--
-- @since 0.1.6
withBinaryFileDurable ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileDurable absFp iomode cb =
#if WINDOWS
  withFile absFp iomode cb
#else
  withRunInIO $ \run ->
    bracket
      (openFileAndDirectory absFp iomode)
      (uncurry closeFileDurable)
      (run . cb . snd)
#endif

-- | Opens a file with the following guarantees:
--
-- * It successfully closes the file in case of an asynchronous exception
--
-- * It reliably saves the file in the correct directory; including edge case
--   situations like a different device being mounted to the current directory,
--   or the current directory being renamed to some other name while the file is
--   being used.
--
-- * It ensures durability by executing an fsync call before closing the file
--   handle
--
-- * It keeps all changes in a copy file, and after is closed it renames it to
--   the original filepath, in case of catastrophic failure, the original file
--   stays unaffected.
--
--
-- === Performance Considerations
--
-- When using a non read-only 'IOMode' (e.g. 'WriteMode', 'ReadWriteMode',
-- 'AppendMode'), this function performs a copy operation of the specified input
-- file to guarantee the original file is intact in case of a catastrophic
-- failure (no partial writes). This approach may be prohibitive in scenarios
-- where the input file is expected to be large in size.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'System.IO.withFile' on Windows platforms.
--
-- @since 0.1.6
withBinaryFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileDurableAtomic absFp iomode cb = do
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
        tmpFp <- toTmpFilePath absFp
        when fileExists $ copyFile absFp tmpFp

        withDurableAtomic tmpFp run
  where
    withDurableAtomic tmpFp run = do
      bracket
        (openFileAndDirectory tmpFp iomode)
        (uncurry $ closeFileDurableAtomic tmpFp absFp)
        (run . cb . snd)
#endif
