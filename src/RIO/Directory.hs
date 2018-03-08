module RIO.Directory
   (
    -- * Actions on directories
      createDirectory
    , createDirectoryIfMissing
    , removeDirectory
    , removeDirectoryRecursive
    , removePathForcibly
    , renameDirectory
    , listDirectory
    , getDirectoryContents
    -- ** Current working directory
    , getCurrentDirectory
    , setCurrentDirectory
    , withCurrentDirectory

    -- * Pre-defined directories
    , getHomeDirectory
    , System.Directory.XdgDirectory(..)
    , getXdgDirectory
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile
    , renameFile
    , renamePath
    , copyFile
    , copyFileWithMetadata

    , canonicalizePath
    , makeAbsolute
    , makeRelativeToCurrentDirectory
    , findExecutable
    , findExecutables
    , findExecutablesInDirectories
    , findFile
    , findFiles
    , findFileWith
    , findFilesWith
    , System.Directory.exeExtension

    , getFileSize

    -- * Existence tests
    , doesPathExist
    , doesFileExist
    , doesDirectoryExist

    -- * Symbolic links
    , pathIsSymbolicLink

    -- * Permissions
    , System.Directory.Permissions
    , System.Directory.emptyPermissions
    , System.Directory.readable
    , System.Directory.writable
    , System.Directory.executable
    , System.Directory.searchable
    , System.Directory.setOwnerReadable
    , System.Directory.setOwnerWritable
    , System.Directory.setOwnerExecutable
    , System.Directory.setOwnerSearchable

    , getPermissions
    , setPermissions
    , copyPermissions

    -- * Timestamps

    , getAccessTime
    , getModificationTime
    , setAccessTime
    , setModificationTime
   ) where

import Data.Time.Clock(UTCTime)
import qualified System.Directory
import UnliftIO

-- | Lifted 'System.Directory.createDirectory'
createDirectory :: MonadIO m => FilePath -> m ()
createDirectory = liftIO . System.Directory.createDirectory

-- | Lifted 'System.Directory.createDirectoryIfMissing'
createDirectoryIfMissing :: MonadIO m => Bool -> FilePath -> m ()
createDirectoryIfMissing b = liftIO . System.Directory.createDirectoryIfMissing b

-- | Lifted 'System.Directory.removeDirectory'
removeDirectory :: MonadIO m => FilePath -> m ()
removeDirectory = liftIO . System.Directory.removeDirectory

-- | Lifted 'System.Directory.removeDirectoryRecursive'
removeDirectoryRecursive :: MonadIO m => FilePath -> m ()
removeDirectoryRecursive = liftIO . System.Directory.removeDirectoryRecursive

-- | Lifted 'System.Directory.removePathForcibly'
removePathForcibly :: MonadIO m => FilePath -> m ()
removePathForcibly = liftIO . System.Directory.removePathForcibly

-- | Lifted 'System.Directory.renameDirectory'
renameDirectory :: MonadIO m => FilePath -> FilePath -> m ()
renameDirectory from to = liftIO $ System.Directory.renameDirectory from to

-- | Lifted 'System.Directory.listDirectory'
listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory = liftIO . System.Directory.listDirectory

-- | Lifted 'System.Directory.getDirectoryContents'
getDirectoryContents :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContents = liftIO . System.Directory.getDirectoryContents

-- | Lifted 'System.Directory.getCurrentDirectory'
getCurrentDirectory :: MonadIO m => m FilePath
getCurrentDirectory = liftIO System.Directory.getCurrentDirectory

-- | Lifted 'System.Directory.setCurrentDirectory'
setCurrentDirectory :: MonadIO m => FilePath -> m ()
setCurrentDirectory = liftIO . System.Directory.setCurrentDirectory

-- | Lifted 'System.Directory.withCurrentDirectory'
withCurrentDirectory :: MonadUnliftIO m => FilePath -> m a -> m a
withCurrentDirectory dir action = do
    actionIO <- toIO action
    liftIO $ System.Directory.withCurrentDirectory dir actionIO

-- | Lifted 'System.Directory.getHomeDirectory'
getHomeDirectory :: MonadIO m => m FilePath
getHomeDirectory = liftIO System.Directory.getHomeDirectory

-- | Lifted 'System.Directory.getXdgDirectory'
getXdgDirectory :: MonadIO m => System.Directory.XdgDirectory -> FilePath -> m FilePath
getXdgDirectory xdg = liftIO . System.Directory.getXdgDirectory xdg

-- | Lifted 'System.Directory.getAppUserDataDirectory'
getAppUserDataDirectory :: MonadIO m => FilePath -> m FilePath
getAppUserDataDirectory = liftIO . System.Directory.getAppUserDataDirectory

-- | Lifted 'System.Directory.getUserDocumentsDirectory'
getUserDocumentsDirectory :: MonadIO m => m FilePath
getUserDocumentsDirectory = liftIO System.Directory.getUserDocumentsDirectory

-- | Lifted 'System.Directory.getTemporaryDirectory'
getTemporaryDirectory :: MonadIO m => m FilePath
getTemporaryDirectory = liftIO System.Directory.getTemporaryDirectory

-- | Lifted 'System.Directory.removeFile'
removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . System.Directory.removeFile

-- | Lifted 'System.Directory.renameFile'
renameFile :: MonadIO m => FilePath -> FilePath -> m ()
renameFile from to = liftIO $ System.Directory.renameFile from to

-- | Lifted 'System.Directory.renamePath'
renamePath :: MonadIO m => FilePath -> FilePath -> m ()
renamePath from to = liftIO $ System.Directory.renamePath from to

-- | Lifted 'System.Directory.copyFile'
copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile from to = liftIO $ System.Directory.copyFile from to

-- | Lifted 'System.Directory.copyFileWithMetadata'
copyFileWithMetadata :: MonadIO m => FilePath -> FilePath -> m ()
copyFileWithMetadata from to = liftIO $ System.Directory.copyFileWithMetadata from to

-- | Lifted 'System.Directory.canonicalizePath'
canonicalizePath :: MonadIO m => FilePath -> m FilePath
canonicalizePath = liftIO . System.Directory.canonicalizePath

-- | Lifted 'System.Directory.makeAbsolute'
makeAbsolute :: MonadIO m => FilePath -> m FilePath
makeAbsolute = liftIO . System.Directory.makeAbsolute

-- | Lifted 'System.Directory.makeRelativeToCurrentDirectory'
makeRelativeToCurrentDirectory :: MonadIO m => FilePath -> m FilePath
makeRelativeToCurrentDirectory = liftIO . System.Directory.makeRelativeToCurrentDirectory

-- | Lifted 'System.Directory.findExecutable'
findExecutable :: MonadIO m => String -> m (Maybe FilePath)
findExecutable = liftIO . System.Directory.findExecutable

-- | Lifted 'System.Directory.findExecutables'
findExecutables :: MonadIO m => String -> m [FilePath]
findExecutables = liftIO . System.Directory.findExecutables

-- | Lifted 'System.Directory.findExecutablesInDirectories'
findExecutablesInDirectories :: MonadIO m => [FilePath] -> String -> m [FilePath]
findExecutablesInDirectories dirs = liftIO . System.Directory.findExecutablesInDirectories dirs

-- | Lifted 'System.Directory.findFile'
findFile :: MonadIO m => [FilePath] -> String -> m (Maybe FilePath)
findFile dirs = liftIO . System.Directory.findFile dirs

-- | Lifted 'System.Directory.findFiles'
findFiles :: MonadIO m => [FilePath] -> String -> m [FilePath]
findFiles dirs = liftIO . System.Directory.findFiles dirs

-- | Lifted 'System.Directory.findFileWith'
findFileWith :: MonadUnliftIO m => (FilePath -> m Bool) -> [FilePath] -> String -> m (Maybe FilePath)
findFileWith prop dirs file =
    withRunInIO $ \unlifter -> System.Directory.findFileWith (unlifter . prop) dirs file

-- | Lifted 'System.Directory.findFilesWith'
findFilesWith :: MonadUnliftIO m => (FilePath -> m Bool) -> [FilePath] -> String -> m [FilePath]
findFilesWith prop dirs file =
    withRunInIO $ \unlifter -> System.Directory.findFilesWith (unlifter . prop) dirs file

-- | Lifted 'System.Directory.getFileSize'
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize = liftIO . System.Directory.getFileSize

-- | Lifted 'System.Directory.doesPathExist'
doesPathExist :: MonadIO m => FilePath -> m Bool
doesPathExist = liftIO . System.Directory.doesPathExist

-- | Lifted 'System.Directory.doesFileExist'
doesFileExist :: MonadIO m => FilePath -> m Bool
doesFileExist = liftIO . System.Directory.doesFileExist

-- | Lifted 'System.Directory.doesDirectoryExist'
doesDirectoryExist :: MonadIO m => FilePath -> m Bool
doesDirectoryExist = liftIO . System.Directory.doesDirectoryExist

-- | Lifted 'System.Directory.pathIsSymbolicLink'
pathIsSymbolicLink :: MonadIO m => FilePath -> m Bool
pathIsSymbolicLink = liftIO . System.Directory.pathIsSymbolicLink

-- | Lifted 'System.Directory.getPermissions'
getPermissions :: MonadIO m => FilePath -> m System.Directory.Permissions
getPermissions = liftIO . System.Directory.getPermissions

-- | Lifted 'System.Directory.setPermissions'
setPermissions :: MonadIO m => FilePath -> System.Directory.Permissions -> m ()
setPermissions = liftIO . System.Directory.setPermissions

-- | Lifted 'System.Directory.copyPermissions'
copyPermissions :: MonadIO m => FilePath -> FilePath -> m ()
copyPermissions from to = liftIO $ System.Directory.copyPermissions from to

-- | Lifted 'System.Directory.getAccessTime'
getAccessTime :: MonadIO m => FilePath -> m UTCTime
getAccessTime = liftIO . System.Directory.getAccessTime

-- | Lifted 'System.Directory.getModificationTime'
getModificationTime :: MonadIO m => FilePath -> m UTCTime
getModificationTime = liftIO . System.Directory.getModificationTime

-- | Lifted 'System.Directory.setAccessTime'
setAccessTime :: MonadIO m => FilePath -> UTCTime -> m ()
setAccessTime path = liftIO . System.Directory.setAccessTime path

-- | Lifted 'System.Directory.setModificationTime'
setModificationTime :: MonadIO m => FilePath -> UTCTime -> m ()
setModificationTime path = liftIO . System.Directory.setModificationTime path

-- isSymbolicLink omitted as it is a deprecated alias of pathIsSymbolicLink
