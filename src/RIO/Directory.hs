{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.Directory
  ( module System.Directory
  , module RIO.Directory
  ) where

import System.Directory hiding (
    canonicalizePath
  , copyFile
  , copyFileWithMetadata
  , copyPermissions
  , createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , findExecutables
  , findExecutablesInDirectories
  , findFile
  , findFileWith
  , findFiles
  , findFilesWith
  , getAccessTime
  , getAppUserDataDirectory
  , getCurrentDirectory
  , getDirectoryContents
  , getHomeDirectory
  , getModificationTime
  , getPermissions
  , getTemporaryDirectory
  , getUserDocumentsDirectory
  , getXdgDirectory
  , isSymbolicLink
  , listDirectory
  , makeAbsolute
  , makeRelativeToCurrentDirectory
  , removeDirectory
  , removeDirectoryRecursive
  , removeFile
  , removePathForcibly
  , renameDirectory
  , renameFile
  , setAccessTime
  , setCurrentDirectory
  , setModificationTime
  , setPermissions
  , withCurrentDirectory
#if MIN_VERSION_directory(1, 3, 0)
  , doesPathExist
  , getFileSize
  , pathIsSymbolicLink
  , removePathForcibly
  , renamePath
#endif
  )

import qualified System.Directory as Directory
import RIO
import RIO.Time (UTCTime)

-- | Lifted 'Directory.canonicalizePath'
canonicalizePath :: MonadIO m => FilePath -> m FilePath
canonicalizePath = liftIO . Directory.canonicalizePath

-- | Lifted 'Directory.copyFile'
copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile fp1 fp2 = liftIO $ Directory.copyFile fp1 fp2

-- | Lifted 'Directory.copyFileWithMetadata'
copyFileWithMetadata :: MonadIO m => FilePath -> FilePath -> m ()
copyFileWithMetadata fp1 fp2 = liftIO $ Directory.copyFileWithMetadata fp1 fp2

-- | Lifted 'Directory.copyPermissions'
copyPermissions :: MonadIO m => FilePath -> FilePath -> m ()
copyPermissions fp1 fp2 = liftIO $ Directory.copyPermissions fp1 fp2

-- | Lifted 'Directory.createDirectory'
createDirectory :: MonadIO m => FilePath -> m ()
createDirectory = liftIO . Directory.createDirectory

-- | Lifted 'Directory.createDirectoryIfMissing'
createDirectoryIfMissing :: MonadIO m => Bool -> FilePath -> m ()
createDirectoryIfMissing b fp = liftIO $ Directory.createDirectoryIfMissing b fp

-- | Lifted 'Directory.doesDirectoryExist'
doesDirectoryExist :: MonadIO m => FilePath -> m Bool
doesDirectoryExist = liftIO . Directory.doesDirectoryExist

-- | Lifted 'Directory.doesFileExist'
doesFileExist :: MonadIO m => FilePath -> m Bool
doesFileExist = liftIO . Directory.doesFileExist

-- | Lifted 'Directory.findExecutable'
findExecutable :: MonadIO m => String -> m (Maybe FilePath)
findExecutable = liftIO . Directory.findExecutable

-- | Lifted 'Directory.findExecutables'
findExecutables :: MonadIO m => String -> m [FilePath]
findExecutables = liftIO . Directory.findExecutables

-- | Lifted 'Directory.findExecutablesInDirectories'
findExecutablesInDirectories :: MonadIO m => [FilePath] -> String -> m [FilePath]
findExecutablesInDirectories fpList str = liftIO $ Directory.findExecutablesInDirectories fpList str

-- | Lifted 'Directory.findFile'
findFile :: MonadIO m => [FilePath] -> String -> m (Maybe FilePath)
findFile fpList str = liftIO $ Directory.findFile fpList str

-- | Unlifted 'Directory.findFileWith'
findFileWith
  :: (MonadIO m, MonadUnliftIO m)
  => (FilePath -> m Bool) -> [FilePath] -> String -> m (Maybe FilePath)
findFileWith inner fpList s =
  withRunInIO $ \run -> Directory.findFileWith (run . inner) fpList s

-- | Lifted 'Directory.findFiles'
findFiles :: MonadIO m => [FilePath] -> String -> m [FilePath]
findFiles fpList s = liftIO $ Directory.findFiles fpList s

-- | Unlifted 'Directory.findFilesWith'
findFilesWith
  :: (MonadIO m, MonadUnliftIO m)
  => (FilePath -> m Bool) -> [FilePath] -> String -> m [FilePath]
findFilesWith inner fpList s =
  withRunInIO $ \run -> Directory.findFilesWith (run . inner) fpList s

-- | Lifted 'Directory.getAccessTime'
getAccessTime :: MonadIO m => FilePath -> m UTCTime
getAccessTime = liftIO . Directory.getAccessTime

-- | Lifted 'Directory.getAppUserDataDirectory'
getAppUserDataDirectory :: MonadIO m => FilePath -> m FilePath
getAppUserDataDirectory = liftIO . Directory.getAppUserDataDirectory

-- | Lifted 'Directory.getCurrentDirectory'
getCurrentDirectory :: MonadIO m => m FilePath
getCurrentDirectory = liftIO Directory.getCurrentDirectory

-- | Lifted 'Directory.getDirectoryContents'
getDirectoryContents :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContents = liftIO . Directory.getDirectoryContents

-- | Lifted 'Directory.getHomeDirectory'
getHomeDirectory :: MonadIO m => m FilePath
getHomeDirectory = liftIO Directory.getHomeDirectory

-- | Lifted 'Directory.getModificationTime'
getModificationTime :: MonadIO m => FilePath -> m UTCTime
getModificationTime = liftIO . Directory.getModificationTime

-- | Lifted 'Directory.getPermissions'
getPermissions :: MonadIO m => FilePath -> m Permissions
getPermissions = liftIO . Directory.getPermissions

-- | Lifted 'Directory.getTemporaryDirectory'
getTemporaryDirectory :: MonadIO m => m FilePath
getTemporaryDirectory = liftIO Directory.getTemporaryDirectory

-- | Lifted 'Directory.getUserDocumentsDirectory'
getUserDocumentsDirectory :: MonadIO m => m FilePath
getUserDocumentsDirectory = liftIO Directory.getUserDocumentsDirectory

-- | Lifted 'Directory.getXdgDirectory'
getXdgDirectory :: MonadIO m => XdgDirectory -> FilePath -> m FilePath
getXdgDirectory dir fp = liftIO $ Directory.getXdgDirectory dir fp

-- | Lifted 'Directory.listDirectory'
listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory = liftIO . Directory.listDirectory

-- | Lifted 'Directory.makeAbsolute'
makeAbsolute :: MonadIO m => FilePath -> m FilePath
makeAbsolute = liftIO . Directory.makeAbsolute

-- | Lifted 'Directory.makeRelativeToCurrentDirectory'
makeRelativeToCurrentDirectory :: MonadIO m => FilePath -> m FilePath
makeRelativeToCurrentDirectory = liftIO . Directory.makeRelativeToCurrentDirectory

-- | Lifted 'Directory.removeDirectory'
removeDirectory :: MonadIO m => FilePath -> m ()
removeDirectory = liftIO . Directory.removeDirectory

-- | Lifted 'Directory.removeDirectoryRecursive'
removeDirectoryRecursive :: MonadIO m => FilePath -> m ()
removeDirectoryRecursive = liftIO . Directory.removeDirectoryRecursive

-- | Lifted 'Directory.removeFile'
removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . Directory.removeFile

-- | Lifted 'Directory.renameDirectory'
renameDirectory :: MonadIO m => FilePath -> FilePath -> m ()
renameDirectory fp1 fp2 = liftIO $ Directory.renameDirectory fp1 fp2

-- | Lifted 'Directory.renameFile'
renameFile :: MonadIO m => FilePath -> FilePath -> m ()
renameFile fp1 fp2 = liftIO $ Directory.renameFile fp1 fp2

-- | Lifted 'Directory.setAccessTime'
setAccessTime :: MonadIO m => FilePath -> UTCTime -> m ()
setAccessTime fp timestamp = liftIO $ Directory.setAccessTime fp timestamp

-- | Lifted 'Directory.setCurrentDirectory'
setCurrentDirectory :: MonadIO m => FilePath -> m ()
setCurrentDirectory = liftIO . Directory.setCurrentDirectory

-- | Lifted 'Directory.setModificationTime'
setModificationTime :: MonadIO m => FilePath -> UTCTime -> m ()
setModificationTime fp timestamp = liftIO $ Directory.setModificationTime fp timestamp

-- | Lifted 'Directory.setPermissions'
setPermissions :: MonadIO m => FilePath -> Permissions -> m ()
setPermissions fp perms = liftIO $ Directory.setPermissions fp perms

-- | Unlifted 'Directory.withCurrentDirectory'
withCurrentDirectory :: (MonadIO m, MonadUnliftIO m) => FilePath -> m a -> m a
withCurrentDirectory fp inner =
  withRunInIO $ \run -> Directory.withCurrentDirectory fp (run inner)

#if MIN_VERSION_directory(1, 3, 0)

-- | Lifted 'Directory.doesPathExist'
doesPathExist :: MonadIO m => FilePath -> m Bool
doesPathExist = liftIO . Directory.doesPathExist

-- | Lifted 'Directory.getFileSize'
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize = liftIO . Directory.getFileSize

-- | Lifted 'Directory.pathIsSymbolicLink'
pathIsSymbolicLink :: MonadIO m => FilePath -> m Bool
pathIsSymbolicLink = liftIO . Directory.pathIsSymbolicLink

-- | Lifted 'Directory.removePathForcibly'
removePathForcibly :: MonadIO m => FilePath -> m ()
removePathForcibly = liftIO . Directory.removePathForcibly

-- | Lifted 'Directory.renamePath'
renamePath :: MonadIO m => FilePath -> FilePath -> m ()
renamePath fp1 fp2 = liftIO $ Directory.renamePath fp1 fp2

#endif
