{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-|

== Rationale

This module offers functions to handle files that offer better durability and/or
atomicity.

== When to use the functions on this module?

Given the usage of this functions comes at a cost in performance, it is important
to consider what are the use cases that are ideal for each of the functions.

=== Not Durable and not Atomic

For this use case, you want to use the regular functions:

* 'System.IO.withBinaryFile'
* 'RIO.writeFileBinary'

The regular use case for this scenario happens when your program is dealing with
outputs that are never going to be consumed again by your program. For example,
imagine you have a program that generates sales reports for the last month, this
is a report that can be generated quickly; you don't really care if the output
file gets corrupted or lost at one particular execution of your program given
that is cheap to execute the data export program a second time. In other words,
your program doesn't /rely/ on the data contained in this file in order to work.

=== Atomic but not Durable

 Imagine a scenario where your program builds a temporary file that serves as an
intermediate step to a bigger task, like Object files (@.o@) in a compilation
process. The program will use an existing @.o@ file if it is present, or it will
build one from scratch if it is not. The file is not really required, but if it
is present, it *must* be valid and consistent. In this situation, you care about
atomicity, but not durability. You can use the functions for such scenario:

* 'withBinaryFileAtomic'
* 'writeBinaryFileAtomic'

__Note__ - there is a peculiar difference between regular file writing
functionality and the one that is done atomically. Even if the orignal file is
removed while it is being modified, because of atomicity, it will be restored
with all modifications, if any. The reason for this is because a copy of the
file was made prior to modifications and at the end atomically replaced the
existing file at the end. An important consequence of this fact is that whenever
the folder containing the file which is being modified is removed, all bets are
off and all atomic functions will result in an exception.

=== Durable but not Atomic

For this use case, you want to use the functions:

* 'withBinaryFileDurable'
* 'writeBinaryFileDurable'

The regular use case for this scenario happens when your program deals with file
modifications that must be guaranteed to be durable, but you don't care that
changes are consistent. If you use this function, more than likely your program
is ensuring consistency guarantees through other means, for example, SQLite uses
 the Write Ahead Log (WAL) algorithm to ensure changes are atomic at an
application level.

=== Durable and Atomic

For this use case, you can use the functions:

* 'withBinaryFileDurableAtomic'
* 'writeBinaryFileDurableAtomic'

The regular use case for this scenario happens when you want to ensure that
after a program is executed, the modifications done to a file are guaranteed to
be saved, and also that changes are rolled-back in case there is a failure (e.g.
hard reboot, shutdown, etc).

-}
module RIO.File
  (
    writeBinaryFileDurable
  , writeBinaryFileDurableAtomic
  , writeBinaryFileAtomic
  , withBinaryFileDurable
  , withBinaryFileDurableAtomic
  , withBinaryFileAtomic
  , ensureFileDurable
  )
  where

import RIO.Prelude.Reexports

#ifdef WINDOWS
import RIO.Prelude.IO

#else
import Data.Bits (Bits, (.|.))
import Data.Typeable (cast)
import Foreign (allocaBytes)
import Foreign.C (CInt(..), throwErrnoIfMinus1, throwErrnoIfMinus1Retry,
                  throwErrnoIfMinus1Retry_)
import GHC.IO.Device (IODeviceType(RegularFile))
import qualified GHC.IO.Device as Device
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as HandleFD
import RIO.ByteString (hPut)
import System.Directory (removeFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (SeekMode(..), hGetBuf, hPutBuf, openBinaryTempFile)
import System.IO.Error (ioeGetErrorType, isAlreadyExistsError,
                        isDoesNotExistError)
import qualified System.Posix.Files as Posix
import System.Posix.Internals (CFilePath, c_close, c_safe_open, withFilePath)
import System.Posix.Types (CMode(..), Fd(..), FileMode)
#if MIN_VERSION_base(4,9,0)
import qualified GHC.IO.Handle.Types as HandleFD (Handle(..), Handle__(..))
#endif


-- TODO: Add a ticket/pull request to export this symbols from
-- System.Internal.Posix
--
-- NOTE: System.Posix.Internal doesn't re-export this constants so we have to
-- recreate-them here

newtype CFlag =
  CFlag CInt
  deriving (Eq, Show, Bits)

foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_wronly" o_WRONLY :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"   o_RDWR   :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_append" o_APPEND :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CFlag
foreign import ccall unsafe "HsBase.h __hscore_o_noctty" o_NOCTTY :: CFlag

-- After here, we have our own imports

-- On non-Linux operating systems that do not support `O_TMPFILE` the value of
-- `o_TMPFILE` will be 0, which is then used to fallback onto a different
-- implementation of temporary files.
foreign import ccall unsafe "rio.c __rio_o_tmpfile" o_TMPFILE :: CFlag


-- | Whenever Operating System does not support @O_TMPFILE@ flag and anonymous
-- temporary files then `o_TMPFILE` flag will be set to @0@
o_TMPFILE_not_supported :: CFlag
o_TMPFILE_not_supported = CFlag 0

newtype CAt = CAt
  { unCAt :: CInt
  } deriving (Eq, Show, Bits)

foreign import ccall unsafe "rio.c __rio_at_fdcwd" at_FDCWD :: CAt
foreign import ccall unsafe "rio.c __rio_at_symlink_follow" at_SYMLINK_FOLLOW :: CAt
foreign import ccall unsafe "rio.c __rio_s_irusr" s_IRUSR :: CMode
foreign import ccall unsafe "rio.c __rio_s_iwusr" s_IWUSR :: CMode

c_open :: CFilePath -> CFlag -> CMode -> IO CInt
c_open fp (CFlag flags) = c_safe_open fp flags

foreign import ccall safe "fcntl.h openat"
  c_safe_openat :: CInt -> CFilePath -> CInt -> CMode -> IO CInt

c_openat :: DirFd -> CFilePath -> CFlag -> CMode -> IO CInt
c_openat (DirFd (Fd fd)) fp (CFlag flags) = c_safe_openat fd fp flags

foreign import ccall safe "fcntl.h renameat"
  c_safe_renameat :: CInt -> CFilePath -> CInt -> CFilePath -> IO CInt

c_renameat :: DirFd -> CFilePath -> DirFd -> CFilePath -> IO CInt
c_renameat (DirFd (Fd fdFrom)) cFpFrom (DirFd (Fd fdTo)) cFdTo =
  c_safe_renameat fdFrom cFpFrom fdTo cFdTo

foreign import ccall safe "unistd.h fsync"
  c_safe_fsync :: CInt -> IO CInt

c_fsync :: Fd -> IO CInt
c_fsync (Fd fd) = c_safe_fsync fd

foreign import ccall safe "unistd.h linkat"
  c_safe_linkat :: CInt -> CFilePath -> CInt -> CFilePath -> CInt -> IO CInt

c_linkat :: CAt -> CFilePath -> Either DirFd CAt -> CFilePath -> CAt -> IO CInt
c_linkat cat oldPath eNewDir newPath (CAt flags) =
  c_safe_linkat (unCAt cat) oldPath newDir newPath flags
  where
    unFd (Fd fd) = fd
    newDir = either (unFd . unDirFd) unCAt eNewDir

std_flags, output_flags, read_flags, write_flags, rw_flags,
    append_flags :: CFlag
std_flags    = o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND

ioModeToFlags :: IOMode -> CFlag
ioModeToFlags iomode =
  case iomode of
    ReadMode      -> read_flags
    WriteMode     -> write_flags
    ReadWriteMode -> rw_flags
    AppendMode    -> append_flags

newtype DirFd = DirFd
  { unDirFd :: Fd
  }

-- | Returns a low-level file descriptor for a directory path. This function
-- exists given the fact that 'openFile' does not work with directories.
--
-- If you use this function, make sure you are working on a masked state,
-- otherwise async exceptions may leave file descriptors open.
openDir :: MonadIO m => FilePath -> m Fd
openDir fp
  -- TODO: Investigate what is the situation with Windows FS in regards to non_blocking
  -- NOTE: File operations _do not support_ non_blocking on various kernels, more
  -- info can be found here: https://ghc.haskell.org/trac/ghc/ticket/15153
 =
  liftIO $
  withFilePath fp $ \cFp ->
    Fd <$>
    throwErrnoIfMinus1Retry
      "openDir"
      (c_open cFp (ioModeToFlags ReadMode) 0o660)

-- | Closes a 'Fd' that points to a Directory.
closeDirectory :: MonadIO m => DirFd -> m ()
closeDirectory (DirFd (Fd dirFd)) =
  liftIO $
  throwErrnoIfMinus1Retry_ "closeDirectory" $ c_close dirFd

-- | Executes the low-level C function fsync on a C file descriptor
fsyncFileDescriptor
  :: MonadIO m
  => String -- ^ Meta-description for error messages
  -> Fd   -- ^ C File Descriptor
  -> m ()
fsyncFileDescriptor name fd =
  liftIO $ void $ throwErrnoIfMinus1 ("fsync - " <> name) $ c_fsync fd

-- | Call @fsync@ on the file handle. Accepts an arbitary string for error reporting.
fsyncFileHandle :: String -> Handle -> IO ()
fsyncFileHandle fname hdl = withHandleFd hdl (fsyncFileDescriptor (fname ++ "/File"))


-- | Call @fsync@ on the opened directory file descriptor. Accepts an arbitary
-- string for error reporting.
fsyncDirectoryFd :: String -> DirFd -> IO ()
fsyncDirectoryFd fname = fsyncFileDescriptor (fname ++ "/Directory") . unDirFd


-- | Opens a file from a directory, using this function in favour of a regular
-- 'openFile' guarantees that any file modifications are kept in the same
-- directory where the file was opened. An edge case scenario is a mount
-- happening in the directory where the file was opened while your program is
-- running.
--
-- If you use this function, make sure you are working on an masked state,
-- otherwise async exceptions may leave file descriptors open.
--
openFileFromDir :: MonadIO m => DirFd -> FilePath -> IOMode -> m Handle
openFileFromDir dirFd filePath@(takeFileName -> fileName) iomode =
  liftIO $
  withFilePath fileName $ \cFileName ->
    bracketOnError
      (do fileFd <-
            throwErrnoIfMinus1Retry "openFileFromDir" $
            c_openat dirFd cFileName (ioModeToFlags iomode) 0o666
            {- Can open directory with read only -}
          FD.mkFD
            fileFd
            iomode
            Nothing {- no stat -}
            False {- not a socket -}
            False {- non_blocking -}
           `onException`
            c_close fileFd)
      (liftIO . Device.close . fst)
      (\(fD, fd_type)
         -- we want to truncate() if this is an open in WriteMode, but only if the
         -- target is a RegularFile. ftruncate() fails on special files like
         -- /dev/null.
        -> do
         when (iomode == WriteMode && fd_type == RegularFile) $
           Device.setSize fD 0
         HandleFD.mkHandleFromFD fD fd_type filePath iomode False Nothing)


-- | Similar to `openFileFromDir`, but will open an anonymous (nameless)
-- temporary file in the supplied directory
openAnonymousTempFileFromDir ::
     MonadIO m =>
     Maybe DirFd
     -- ^ If the file descriptor for the directory the target file is/will be
     -- located in, than it will be used for opening an anonymous file
     -> FilePath
     -- ^ File path of the target file that we are working on.
     -> IOMode
     -> m Handle
openAnonymousTempFileFromDir mDirFd filePath iomode =
  liftIO $
  case mDirFd of
    Just dirFd -> withFilePath "." (openAnonymousWith . c_openat dirFd)
    Nothing ->
      withFilePath (takeDirectory filePath) (openAnonymousWith . c_open)
  where
    fdName = "openAnonymousTempFileFromDir - " ++ filePath
    ioModeToTmpFlags :: IOMode -> CFlag
    ioModeToTmpFlags =
      \case
        ReadMode -> o_RDWR -- It is an error to create a O_TMPFILE with O_RDONLY
        ReadWriteMode -> o_RDWR
        _ -> o_WRONLY
    openAnonymousWith fopen =
      bracketOnError
        (do fileFd <-
              throwErrnoIfMinus1Retry "openAnonymousTempFileFromDir" $
              fopen (o_TMPFILE .|. ioModeToTmpFlags iomode) (s_IRUSR .|. s_IWUSR)
            FD.mkFD
              fileFd
              iomode
              Nothing {- no stat -}
              False {- not a socket -}
              False {- non_blocking -}
             `onException`
              c_close fileFd)
        (liftIO . Device.close . fst)
        (\(fD, fd_type) ->
           HandleFD.mkHandleFromFD fD fd_type fdName iomode False Nothing)


atomicDurableTempFileRename ::
     DirFd -> Maybe FileMode -> Handle -> Maybe FilePath -> FilePath -> IO ()
atomicDurableTempFileRename dirFd mFileMode tmpFileHandle mTmpFilePath filePath = do
  fsyncFileHandle "atomicDurableTempFileCreate" tmpFileHandle
  -- at this point we know that the content has been persisted to the storage it
  -- is safe to do the atomic move/replace
  let eTmpFile = maybe (Left tmpFileHandle) Right mTmpFilePath
  atomicTempFileRename (Just dirFd) mFileMode eTmpFile filePath
  -- Important to close the handle, so the we can fsync the directory
  hClose tmpFileHandle
  -- file path is updated, now we can fsync the directory
  fsyncDirectoryFd "atomicDurableTempFileCreate" dirFd


-- | There will be an attempt to atomically convert an invisible temporary file
-- into a target file at the supplied file path. In case when there is already a
-- file at that file path, a new visible temporary file will be created in the
-- same folder and then atomically renamed into the target file path, replacing
-- any existing file. This is necessary since `c_safe_linkat` cannot replace
-- files atomically and we have to fall back onto `c_safe_renameat`. This should
-- not be a problem in practice, since lifetime of such visible file is
-- extremely short and it will be cleaned up regardless of the outcome of the
-- rename.
--
-- It is important to note, that whenever a file descriptor for the containing
-- directory is supplied, renaming and linking will be done in its context,
-- thus allowing to do proper fsyncing if durability is necessary.
--
-- __NOTE__: this function will work only on Linux.
--
atomicTempFileCreate ::
     Maybe DirFd
  -- ^ Possible handle for the directory where the target file is located. Which
  -- means that the file is already in that directory, just without a name. In other
  -- words it was opened before with `openAnonymousTempFileFromDir`
  -> Maybe FileMode
  -- ^ If file permissions are supplied they will be set on the new file prior
  -- to atomic rename.
  -> Handle
  -- ^ Handle to the anonymous temporary file created with `c_openat` and
  -- `o_TMPFILE`
  -> FilePath
  -- ^ File path for the target file.
  -> IO ()
atomicTempFileCreate mDirFd mFileMode tmpFileHandle filePath =
  withHandleFd tmpFileHandle $ \fd@(Fd cFd) ->
    withFilePath ("/proc/self/fd/" ++ show cFd) $ \cFromFilePath ->
      withFilePath filePathName $ \cToFilePath -> do
        forM_ mFileMode (Posix.setFdMode fd)
        let safeLink which to =
              throwErrnoIfMinus1Retry_
                ("atomicFileCreate - c_safe_linkat - " ++ which) $
              -- see `man linkat` and `man openat` for more info
              c_linkat at_FDCWD cFromFilePath cDirFd to at_SYMLINK_FOLLOW
        eExc <-
          tryJust (guard . isAlreadyExistsError) $
          safeLink "anonymous" cToFilePath
        case eExc of
          Right () -> pure ()
          Left () ->
            withBinaryTempFileFor filePath $ \visTmpFileName visTmpFileHandle -> do
              hClose visTmpFileHandle
              removeFile visTmpFileName
              case mDirFd of
                Nothing -> do
                  withFilePath visTmpFileName (safeLink "visible")
                  Posix.rename visTmpFileName filePath
                Just dirFd ->
                  withFilePath (takeFileName visTmpFileName) $ \cVisTmpFile -> do
                    safeLink "visible" cVisTmpFile
                    throwErrnoIfMinus1Retry_
                        "atomicFileCreate - c_safe_renameat" $
                      c_renameat dirFd cVisTmpFile dirFd cToFilePath
  where
    (cDirFd, filePathName) =
      case mDirFd of
        Nothing    -> (Right at_FDCWD, filePath)
        Just dirFd -> (Left dirFd, takeFileName filePath)

atomicTempFileRename ::
     Maybe DirFd
     -- ^ Possible handle for the directory where the target file is located.
  -> Maybe FileMode
  -- ^ If file permissions are supplied they will be set on the new file prior
  -- to atomic rename.
  -> Either Handle FilePath
  -- ^ Temporary file. If a handle is supplied, it means it was opened with
  -- @O_TMPFILE@ flag and thus we are on the Linux OS and can safely call
  -- `atomicTempFileCreate`
  -> FilePath
  -- ^ File path for the target file. Whenever `DirFd` is supplied, it must be
  -- the containgin directory fo this file, but that invariant is not enforced
  -- within this function.
  -> IO ()
atomicTempFileRename mDirFd mFileMode eTmpFile filePath =
  case eTmpFile of
    Left tmpFileHandle ->
      atomicTempFileCreate mDirFd mFileMode tmpFileHandle filePath
    Right tmpFilePath -> do
      forM_ mFileMode $ \fileMode -> Posix.setFileMode tmpFilePath fileMode
      case mDirFd of
        Nothing -> Posix.rename tmpFilePath filePath
        Just dirFd ->
          withFilePath (takeFileName filePath) $ \cToFilePath ->
            withFilePath (takeFileName tmpFilePath) $ \cTmpFilePath ->
              throwErrnoIfMinus1Retry_ "atomicFileCreate - c_safe_renameat" $
              c_renameat dirFd cTmpFilePath dirFd cToFilePath


withDirectory :: MonadUnliftIO m => FilePath -> (DirFd -> m a) -> m a
withDirectory dirPath = bracket (DirFd <$> openDir dirPath) closeDirectory

withFileInDirectory ::
     MonadUnliftIO m => DirFd -> FilePath -> IOMode -> (Handle -> m a) -> m a
withFileInDirectory dirFd filePath iomode =
  bracket (openFileFromDir dirFd filePath iomode) hClose


-- | Create a temporary file for a matching possibly exiting target file that
-- will be replaced in the future. Temporary file is meant to be renamed
-- afterwards, thus it is only deleted upon error.
--
-- __Important__: Temporary file is not removed and file handle is not closed if
-- there was no exception thrown by the supplied action.
withBinaryTempFileFor ::
     MonadUnliftIO m
  => FilePath
  -- ^ Source file path. It may exist or may not.
  -> (FilePath -> Handle -> m a)
  -> m a
withBinaryTempFileFor filePath action =
  bracketOnError
    (liftIO (openBinaryTempFile dirPath tmpFileName))
    (\(tmpFilePath, tmpFileHandle) ->
        hClose tmpFileHandle >> liftIO (tryIO (removeFile tmpFilePath)))
    (uncurry action)
  where
    dirPath = takeDirectory filePath
    fileName = takeFileName filePath
    tmpFileName = "." <> fileName <> ".tmp"

-- | Returns `Nothing` if anonymous temporary file is not supported by the OS or
-- the underlying file system can't handle that feature.
withAnonymousBinaryTempFileFor ::
     MonadUnliftIO m
  => Maybe DirFd
  -- ^ It is possible to open the temporary file in the context of a directory,
  -- in such case supply its file descriptor. i.e. @openat@ will be used instead
  -- of @open@
  -> FilePath
  -- ^ Source file path. The file may exist or may not.
  -> IOMode
  -> (Handle -> m a)
  -> m (Maybe a)
withAnonymousBinaryTempFileFor mDirFd filePath iomode action
  | o_TMPFILE == o_TMPFILE_not_supported = pure Nothing
  | otherwise =
    trySupported $
    bracket (openAnonymousTempFileFromDir mDirFd filePath iomode) hClose action
  where
    trySupported m =
      tryIO m >>= \case
        Right res -> pure $ Just res
        Left exc
          | ioeGetErrorType exc == UnsupportedOperation -> pure Nothing
        Left exc -> throwIO exc

withNonAnonymousBinaryTempFileFor ::
     MonadUnliftIO m
  => Maybe DirFd
  -- ^ It is possible to open the temporary file in the context of a directory,
  -- in such case supply its file descriptor. i.e. @openat@ will be used instead
  -- of @open@
  -> FilePath
  -- ^ Source file path. The file may exist or may not.
  -> IOMode
  -> (FilePath -> Handle -> m a)
  -> m a
withNonAnonymousBinaryTempFileFor mDirFd filePath iomode action =
  withBinaryTempFileFor filePath $ \tmpFilePath tmpFileHandle -> do
    hClose tmpFileHandle
    case mDirFd of
      Nothing -> withBinaryFile tmpFilePath iomode (action tmpFilePath)
      Just dirFd -> withFileInDirectory dirFd tmpFilePath iomode (action tmpFilePath)

-- | Copy the contents of the file into the handle, but only if that file exists
-- and either `ReadWriteMode` or `AppendMode` is specified. Returned are the
-- file permissions of the original file so it can be set later when original
-- gets overwritten atomically.
copyFileHandle ::
     MonadUnliftIO f => IOMode -> FilePath -> Handle -> f (Maybe FileMode)
copyFileHandle iomode fromFilePath toHandle =
  either (const Nothing) Just <$>
  tryJust
    (guard . isDoesNotExistError)
    (do fileStatus <- liftIO $ Posix.getFileStatus fromFilePath
        -- Whenever we are not overwriting an existing file, we also need a
        -- copy of the file's contents
        unless (iomode == WriteMode) $ do
          withBinaryFile fromFilePath ReadMode (`copyHandleData` toHandle)
          unless (iomode == AppendMode) $ hSeek toHandle AbsoluteSeek 0
        -- Get the copy of source file permissions, but only whenever it exists
        pure $ Posix.fileMode fileStatus)


-- This is a copy of the internal function from `directory-1.3.3.2`. It became
-- available only in directory-1.3.3.0 and is still internal, hence the
-- duplication.
copyHandleData :: MonadIO m => Handle -> Handle -> m ()
copyHandleData hFrom hTo = liftIO $ allocaBytes bufferSize go
  where
    bufferSize = 131072 -- 128 KiB, as coreutils `cp` uses as of May 2014 (see ioblksize.h)
    go buffer = do
      count <- hGetBuf hFrom buffer bufferSize
      when (count > 0) $ do
        hPutBuf hTo buffer count
        go buffer

-- | Thread safe access to the file descriptor in the file handle
withHandleFd :: Handle -> (Fd -> IO a) -> IO a
withHandleFd h cb =
  case h of
    HandleFD.FileHandle _ mv ->
      withMVar mv $ \HandleFD.Handle__{HandleFD.haDevice = dev} ->
        case cast dev of
          Just fd -> cb $ Fd $ FD.fdFD fd
          Nothing -> error "withHandleFd: not a file handle"
    HandleFD.DuplexHandle {} -> error "withHandleFd: not a file handle"

-- | See `ensureFileDurable`
ensureFileDurablePosix :: MonadIO m => FilePath -> m ()
ensureFileDurablePosix filePath =
  liftIO $
  withDirectory (takeDirectory filePath) $ \dirFd ->
    withFileInDirectory dirFd filePath ReadMode $ \fileHandle ->
      liftIO $ do
        fsyncFileHandle "ensureFileDurablePosix" fileHandle
        -- NOTE: Here we are purposefully not fsyncing the directory if the file fails to fsync
        fsyncDirectoryFd "ensureFileDurablePosix" dirFd



-- | See `withBinaryFileDurable`
withBinaryFileDurablePosix ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileDurablePosix filePath iomode action =
  case iomode of
    ReadMode
      -- We do not need to consider durable operations when we are in a
      -- 'ReadMode', so we can use a regular `withBinaryFile`
     -> withBinaryFile filePath iomode action
    _ {- WriteMode,  ReadWriteMode,  AppendMode -}
     ->
      withDirectory (takeDirectory filePath) $ \dirFd ->
        withFileInDirectory dirFd filePath iomode $ \tmpFileHandle -> do
          res <- action tmpFileHandle
          liftIO $ do
            fsyncFileHandle "withBinaryFileDurablePosix" tmpFileHandle
            -- NOTE: Here we are purposefully not fsyncing the directory if the file fails to fsync
            fsyncDirectoryFd "withBinaryFileDurablePosix" dirFd
          pure res

-- | See `withBinaryFileDurableAtomic`
withBinaryFileDurableAtomicPosix ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileDurableAtomicPosix filePath iomode action =
  case iomode of
    ReadMode
      -- We do not need to consider an atomic operation when we are in a
      -- 'ReadMode', so we can use a regular `withBinaryFile`
     -> withBinaryFile filePath iomode action
    _ {- WriteMode,  ReadWriteMode,  AppendMode -}
     ->
      withDirectory (takeDirectory filePath) $ \dirFd -> do
        mRes <- withAnonymousBinaryTempFileFor (Just dirFd) filePath iomode $
          durableAtomicAction dirFd Nothing
        case mRes of
          Just res -> pure res
          Nothing ->
            withNonAnonymousBinaryTempFileFor (Just dirFd) filePath iomode $ \tmpFilePath ->
                 durableAtomicAction dirFd (Just tmpFilePath)
  where
    durableAtomicAction dirFd mTmpFilePath tmpFileHandle = do
      mFileMode <- copyFileHandle iomode filePath tmpFileHandle
      res <- action tmpFileHandle
      unless (iomode == ReadMode) $
              -- For `ReadMode` all we need is isolation.
        liftIO $
        atomicDurableTempFileRename
          dirFd
          mFileMode
          tmpFileHandle
          mTmpFilePath
          filePath
      pure res

-- | See `withBinaryFileAtomic`
withBinaryFileAtomicPosix ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFileAtomicPosix filePath iomode action =
  case iomode of
    ReadMode
      -- We do not need to consider an atomic operation when we are in a
      -- 'ReadMode', so we can use a regular `withBinaryFile`
     -> withBinaryFile filePath iomode action
    _ {- WriteMode,  ReadWriteMode,  AppendMode -}
     -> do
      mRes <-
        withAnonymousBinaryTempFileFor Nothing filePath iomode $
        atomicAction Nothing
      case mRes of
        Just res -> pure res
        Nothing ->
          withNonAnonymousBinaryTempFileFor Nothing filePath iomode $ \tmpFilePath ->
            atomicAction (Just tmpFilePath)
  where
    atomicAction mTmpFilePath tmpFileHandle = do
      let eTmpFile = maybe (Left tmpFileHandle) Right mTmpFilePath
      mFileMode <- copyFileHandle iomode filePath tmpFileHandle
      res <- action tmpFileHandle
      liftIO $ atomicTempFileRename Nothing mFileMode eTmpFile filePath
      pure res

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
ensureFileDurable :: MonadIO m => FilePath -> m ()
-- Implementation is at the bottom of the module


-- | Similar to 'writeFileBinary', but it also ensures that changes executed to
-- the file are guaranteed to be durable. It internally uses fsync and makes
-- sure it synchronizes the file on disk.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'RIO.writeFileBinary' on Windows platforms.
--
-- @since 0.1.6
writeBinaryFileDurable :: MonadIO m => FilePath -> ByteString -> m ()
-- Implementation is at the bottom of the module

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
writeBinaryFileDurableAtomic :: MonadIO m => FilePath -> ByteString -> m ()
-- Implementation is at the bottom of the module

-- | Same as 'writeBinaryFileDurableAtomic', except it does not guarantee durability.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'RIO.writeFileBinary' on Windows platforms.
--
-- @since 0.1.10
writeBinaryFileAtomic :: MonadIO m => FilePath -> ByteString -> m ()
-- Implementation is at the bottom of the module

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
-- This function behaves the same as 'System.IO.withBinaryFile' on Windows platforms.
--
-- @since 0.1.6
withBinaryFileDurable ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
-- Implementation is at the bottom of the module

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
-- * It keeps all changes in a temporary file, and after it is closed it atomically
--   moves the temporary file to the original filepath, in case of catastrophic
--   failure, the original file stays unaffected.
--
-- If you do not need durability but only atomicity, use `withBinaryFileAtomic`
-- instead, which is faster as it does not perform @fsync()@.
--
-- __Important__ - Make sure not to close the `Handle`, otherwise it will result
-- in @invalid argument (Bad file descriptor)@ exception
--
-- === Performance Considerations
--
-- When using a writable but non-truncating 'IOMode' (i.e. 'ReadWriteMode' and
-- 'AppendMode'), this function performs a copy operation of the specified input
-- file to guarantee the original file is intact in case of a catastrophic
-- failure (no partial writes). This approach may be prohibitive in scenarios
-- where the input file is expected to be large in size.
--
-- === Cross-Platform support
--
-- This function behaves the same as 'System.IO.withBinaryFile' on Windows
-- platforms.
--
-- @since 0.1.6
withBinaryFileDurableAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
-- Implementation is at the bottom of the module


-- | Perform an action on a new or existing file at the destination file path. If
-- previously the file existed at the supplied file path then:
--
-- * in case of `WriteMode` it will be overwritten
--
-- * upon `ReadWriteMode` or `AppendMode` files contents will be copied over
-- into a temporary file, thus making sure no corruption can happen to an
-- existing file upon any failures, even catastrophic one, yet its contents are
-- availble for modification.
--
-- * There is nothing atomic about `ReadMode`, so no special treatment there.
--
-- It is similar to `withBinaryFileDurableAtomic`, but without the durability
-- part. It means that all modification can still disappear after it has
-- been succesfully written due to some extreme event like an abrupt power loss,
-- but the contents will not be corrupted in case when the file write did not
-- end successfully.
--
-- The same performance caveats apply as for `withBinaryFileDurableAtomic` due
-- to making a copy of the content of existing files during non-truncating
-- writes.
--
-- __Important__ - Do not close the handle, otherwise it will result in @invalid
-- argument (Bad file descriptor)@ exception
--
-- @since 0.1.10
withBinaryFileAtomic ::
     MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
-- Implementation is at the bottom of the module


#if WINDOWS
ensureFileDurable = (`seq` pure ())

writeBinaryFileDurable = writeFileBinary
writeBinaryFileDurableAtomic = writeFileBinary
writeBinaryFileAtomic = writeFileBinary

withBinaryFileDurable = withBinaryFile
withBinaryFileDurableAtomic = withBinaryFile
withBinaryFileAtomic = withBinaryFile
#else
ensureFileDurable = ensureFileDurablePosix

writeBinaryFileDurable fp bytes =
  liftIO $ withBinaryFileDurable fp WriteMode (`hPut` bytes)
writeBinaryFileDurableAtomic fp bytes =
  liftIO $ withBinaryFileDurableAtomic fp WriteMode (`hPut` bytes)
writeBinaryFileAtomic fp bytes =
  liftIO $ withBinaryFileAtomic fp WriteMode (`hPut` bytes)

withBinaryFileDurable = withBinaryFileDurablePosix
withBinaryFileDurableAtomic = withBinaryFileDurableAtomicPosix
withBinaryFileAtomic = withBinaryFileAtomicPosix
#endif
