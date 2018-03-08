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

    -- * Deprecated
    , isSymbolicLink

   ) where

-- | Lifted 'System.Directory.createDirectory'
createDirectory :: _
createDirectory = undefined

-- | Lifted 'System.Directory.createDirectoryIfMissing'
createDirectoryIfMissing :: _
createDirectoryIfMissing = undefined

-- | Lifted 'System.Directory.removeDirectory'
removeDirectory :: _
removeDirectory = undefined

-- | Lifted 'System.Directory.removeDirectoryRecursive'
removeDirectoryRecursive :: _
removeDirectoryRecursive = undefined

-- | Lifted 'System.Directory.removePathForcibly'
removePathForcibly :: _
removePathForcibly = undefined

-- | Lifted 'System.Directory.renameDirectory'
renameDirectory :: _
renameDirectory = undefined

-- | Lifted 'System.Directory.listDirectory'
listDirectory :: _
listDirectory = undefined

-- | Lifted 'System.Directory.getDirectoryContents'
getDirectoryContents :: _
getDirectoryContents = undefined

-- | Lifted 'System.Directory.getCurrentDirectory'
getCurrentDirectory :: _
getCurrentDirectory = undefined

-- | Lifted 'System.Directory.setCurrentDirectory'
setCurrentDirectory :: _
setCurrentDirectory = undefined

-- | Lifted 'System.Directory.withCurrentDirectory'
withCurrentDirectory :: _
withCurrentDirectory = undefined

-- | Lifted 'System.Directory.getHomeDirectory'
getHomeDirectory :: _
getHomeDirectory = undefined

-- | Lifted 'System.Directory.getXdgDirectory'
getXdgDirectory :: _
getXdgDirectory = undefined

-- | Lifted 'System.Directory.getAppUserDataDirectory'
getAppUserDataDirectory :: _
getAppUserDataDirectory = undefined

-- | Lifted 'System.Directory.getUserDocumentsDirectory'
getUserDocumentsDirectory :: _
getUserDocumentsDirectory = undefined

-- | Lifted 'System.Directory.getTemporaryDirectory'
getTemporaryDirectory :: _
getTemporaryDirectory = undefined

-- | Lifted 'System.Directory.removeFile'
removeFile :: _
removeFile = undefined

-- | Lifted 'System.Directory.renameFile'
renameFile :: _
renameFile = undefined

-- | Lifted 'System.Directory.renamePath'
renamePath :: _
renamePath = undefined

-- | Lifted 'System.Directory.copyFile'
copyFile :: _
copyFile = undefined

-- | Lifted 'System.Directory.copyFileWithMetadata'
copyFileWithMetadata :: _
copyFileWithMetadata = undefined

-- | Lifted 'System.Directory.canonicalizePath'
canonicalizePath :: _
canonicalizePath = undefined

-- | Lifted 'System.Directory.makeAbsolute'
makeAbsolute :: _
makeAbsolute = undefined

-- | Lifted 'System.Directory.makeRelativeToCurrentDirectory'
makeRelativeToCurrentDirectory :: _
makeRelativeToCurrentDirectory = undefined

-- | Lifted 'System.Directory.findExecutable'
findExecutable :: _
findExecutable = undefined

-- | Lifted 'System.Directory.findExecutables'
findExecutables :: _
findExecutables = undefined

-- | Lifted 'System.Directory.findExecutablesInDirectories'
findExecutablesInDirectories :: _
findExecutablesInDirectories = undefined

-- | Lifted 'System.Directory.findFile'
findFile :: _
findFile = undefined

-- | Lifted 'System.Directory.findFiles'
findFiles :: _
findFiles = undefined

-- | Lifted 'System.Directory.findFileWith'
findFileWith :: _
findFileWith = undefined

-- | Lifted 'System.Directory.findFilesWith'
findFilesWith :: _
findFilesWith = undefined

-- | Lifted 'System.Directory.getFileSize'
getFileSize :: _
getFileSize = undefined

-- | Lifted 'System.Directory.doesPathExist'
doesPathExist :: _
doesPathExist = undefined

-- | Lifted 'System.Directory.doesFileExist'
doesFileExist :: _
doesFileExist = undefined

-- | Lifted 'System.Directory.doesDirectoryExist'
doesDirectoryExist :: _
doesDirectoryExist = undefined

-- | Lifted 'System.Directory.pathIsSymbolicLink'
pathIsSymbolicLink :: _
pathIsSymbolicLink = undefined

-- | Lifted 'System.Directory.getPermissions'
getPermissions :: _
getPermissions = undefined

-- | Lifted 'System.Directory.setPermissions'
setPermissions :: _
setPermissions = undefined

-- | Lifted 'System.Directory.copyPermissions'
copyPermissions :: _
copyPermissions = undefined

-- | Lifted 'System.Directory.getAccessTime'
getAccessTime :: _
getAccessTime = undefined

-- | Lifted 'System.Directory.getModificationTime'
getModificationTime :: _
getModificationTime = undefined

-- | Lifted 'System.Directory.setAccessTime'
setAccessTime :: _
setAccessTime = undefined

-- | Lifted 'System.Directory.setModificationTime'
setModificationTime :: _
setModificationTime = undefined

-- | Lifted 'System.Directory.isSymbolicLink'
isSymbolicLink :: _
isSymbolicLink = undefined
