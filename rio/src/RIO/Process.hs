{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Interacting with external processes.
--
-- This module provides a layer on top of "System.Process.Typed", with
-- the following additions:
--
-- * For efficiency, it will cache @PATH@ lookups.
--
-- * For convenience, you can set the working directory and env vars
--   overrides in a 'RIO' environment instead of on the individual
--   calls to the process.
--
-- * Built-in support for logging at the debug level.
--
-- In order to switch over to this API, the main idea is:
--
-- * Like most of the rio library, you need to create an environment
--   value (this time 'ProcessContext'), and include it in your 'RIO'
--   environment. See 'mkProcessContext'.
--
-- * Instead of using the 'System.Process.Typed.proc' function from
--   "System.Process.Typed" for creating a 'ProcessConfig', use the
--   locally defined 'proc' function, which will handle overriding
--   environment variables, looking up paths, performing logging, etc.
--
-- Once you have your 'ProcessConfig', use the standard functions from
-- 'System.Process.Typed' (reexported here for convenient) for running
-- the 'ProcessConfig'.
--
-- @since 0.0.3.0
module RIO.Process
  ( -- * Process context
    ProcessContext
  , HasProcessContext (..)
  , EnvVars
  , mkProcessContext
  , mkDefaultProcessContext
  , modifyEnvVars
  , withModifyEnvVars
  , lookupEnvFromContext
  , withWorkingDir
    -- ** Lenses
  , workingDirL
  , envVarsL
  , envVarsStringsL
  , exeSearchPathL
    -- ** Actions
  , resetExeCache
    -- * Configuring
  , proc
    -- * Spawning (run child process)
  , withProcess
  , withProcess_
  , withProcessWait
  , withProcessWait_
  , withProcessTerm
  , withProcessTerm_
    -- * Exec (replacing current process)
  , exec
  , execSpawn
    -- * Environment helper
  , LoggedProcessContext (..)
  , withProcessContextNoLogging
    -- * Exceptions
  , ProcessException (..)
    -- * Utilities
  , doesExecutableExist
  , findExecutable
  , exeExtensions
  , augmentPath
  , augmentPathMap
  , augmentPathMap'
  , showProcessArgDebug
    -- * Reexports
  , P.ProcessConfig
  , P.StreamSpec
  , P.StreamType (..)
  , P.Process
  , P.setStdin
  , P.setStdout
  , P.setStderr
  , P.setCloseFds
  , P.setCreateGroup
  , P.setDelegateCtlc
#if MIN_VERSION_process(1, 3, 0)
  , P.setDetachConsole
  , P.setCreateNewConsole
  , P.setNewSession
#endif
#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
  , P.setChildGroup
  , P.setChildUser
#endif
  , P.mkStreamSpec
  , P.inherit
  , P.closed
  , P.byteStringInput
  , P.byteStringOutput
  , P.createPipe
  , P.useHandleOpen
  , P.useHandleClose
  , P.startProcess
  , P.stopProcess
  , P.readProcess
  , P.readProcess_
  , P.runProcess
  , P.runProcess_
  , P.readProcessStdout
  , P.readProcessStdout_
  , P.readProcessStderr
  , P.readProcessStderr_
  , P.waitExitCode
  , P.waitExitCodeSTM
  , P.getExitCode
  , P.getExitCodeSTM
  , P.checkExitCode
  , P.checkExitCodeSTM
  , P.getStdin
  , P.getStdout
  , P.getStderr
  , P.ExitCodeException (..)
  , P.ByteStringOutputException (..)
  , P.unsafeProcessHandle
  ) where

import           RIO.Prelude.Display
import           RIO.Prelude.Reexports
import           RIO.Prelude.Logger
import           RIO.Prelude.RIO
import           RIO.Prelude.Lens
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.Directory as D
import           System.Environment (getEnvironment)
import           System.Exit (exitWith)
import qualified System.FilePath as FP
import qualified System.Process.Typed as P
import           System.Process.Typed hiding
                    (withProcess, withProcess_,
                     withProcessWait, withProcessWait_,
                     withProcessTerm, withProcessTerm_,
                     proc)

#ifndef WINDOWS
import           System.Directory (setCurrentDirectory)
import           System.Posix.Process (executeFile)
#endif

-- | The environment variable map
--
-- @since 0.0.3.0
type EnvVars = Map Text Text

-- | Context in which to run processes.
--
-- @since 0.0.3.0
data ProcessContext = ProcessContext
    { pcTextMap :: !EnvVars
    -- ^ Environment variables as map

    , pcStringList :: ![(String, String)]
    -- ^ Environment variables as association list

    , pcPath :: ![FilePath]
    -- ^ List of directories searched for executables (@PATH@)

    , pcExeCache :: !(IORef (Map FilePath (Either ProcessException FilePath)))
    -- ^ Cache of already looked up executable paths.

    , pcExeExtensions :: [String]
    -- ^ @[""]@ on non-Windows systems, @["", ".exe", ".bat"]@ on Windows

    , pcWorkingDir :: !(Maybe FilePath)
    -- ^ Override the working directory.
    }

-- | Exception type which may be generated in this module.
--
-- /NOTE/ Other exceptions may be thrown by underlying libraries!
--
-- @since 0.0.3.0
data ProcessException
    = NoPathFound
    | ExecutableNotFound String [FilePath]
    | ExecutableNotFoundAt FilePath
    | PathsInvalidInPath [FilePath]
    deriving (Typeable, Eq)
instance Show ProcessException where
    show NoPathFound = "PATH not found in ProcessContext"
    show (ExecutableNotFound name path) = concat
        [ "Executable named "
        , name
        , " not found on path: "
        , show path
        ]
    show (ExecutableNotFoundAt name) =
        "Did not find executable at specified path: " ++ name
    show (PathsInvalidInPath paths) = unlines $
        [ "Would need to add some paths to the PATH environment variable \
          \to continue, but they would be invalid because they contain a "
          ++ show FP.searchPathSeparator ++ "."
        , "Please fix the following paths and try again:"
        ] ++ paths
instance Exception ProcessException

-- | Get the 'ProcessContext' from the environment.
--
-- @since 0.0.3.0
class HasProcessContext env where
  processContextL :: Lens' env ProcessContext
instance HasProcessContext ProcessContext where
  processContextL = id

data EnvVarFormat = EVFWindows | EVFNotWindows

currentEnvVarFormat :: EnvVarFormat
currentEnvVarFormat =
#if WINDOWS
  EVFWindows
#else
  EVFNotWindows
#endif

-- Don't use CPP so that the Windows code path is at least type checked
-- regularly
isWindows :: Bool
isWindows = case currentEnvVarFormat of
              EVFWindows -> True
              EVFNotWindows -> False

-- | Override the working directory processes run in. @Nothing@ means
-- the current process's working directory.
--
-- @since 0.0.3.0
workingDirL :: HasProcessContext env => Lens' env (Maybe FilePath)
workingDirL = processContextL.lens pcWorkingDir (\x y -> x { pcWorkingDir = y })

-- | Get the environment variables. We cannot provide a @Lens@ here,
-- since updating the environment variables requires an @IO@ action to
-- allocate a new @IORef@ for holding the executable path cache.
--
-- @since 0.0.3.0
envVarsL :: HasProcessContext env => SimpleGetter env EnvVars
envVarsL = processContextL.to pcTextMap

-- | Get the 'EnvVars' as an associated list of 'String's.
--
-- Useful for interacting with other libraries.
--
-- @since 0.0.3.0
envVarsStringsL :: HasProcessContext env => SimpleGetter env [(String, String)]
envVarsStringsL = processContextL.to pcStringList

-- | Get the list of directories searched for executables (the @PATH@).
--
-- Similar to 'envVarMapL', this cannot be a full @Lens@.
--
-- @since 0.0.3.0
exeSearchPathL :: HasProcessContext env => SimpleGetter env [FilePath]
exeSearchPathL = processContextL.to pcPath

-- | Create a new 'ProcessContext' from the given environment variable map.
--
-- @since 0.0.3.0
mkProcessContext :: MonadIO m => EnvVars -> m ProcessContext
mkProcessContext (normalizePathEnv -> tm) = do
    ref <- newIORef Map.empty
    return ProcessContext
        { pcTextMap = tm
        , pcStringList = map (T.unpack *** T.unpack) $ Map.toList tm
        , pcPath =
             (if isWindows then (".":) else id)
             (maybe [] (FP.splitSearchPath . T.unpack) (Map.lookup "PATH" tm))
        , pcExeCache = ref
        , pcExeExtensions =
            if isWindows
                then let pathext = fromMaybe defaultPATHEXT
                                             (Map.lookup "PATHEXT" tm)
                      in map T.unpack $ T.splitOn ";" pathext
                else [""]
        , pcWorkingDir = Nothing
        }
  where
    -- Default value for PATHTEXT on Windows versions after Windows XP. (The
    -- documentation of the default at
    -- https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/start
    -- is incomplete.)
    defaultPATHEXT = ".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC"


-- Fix case insensitivity of the PATH environment variable on Windows,
-- by forcing all keys full uppercase.
normalizePathEnv :: EnvVars -> EnvVars
normalizePathEnv env
  | isWindows = Map.fromList $ map (first T.toUpper) $ Map.toList env
  | otherwise = env


-- | Reset the executable cache.
--
-- @since 0.0.3.0
resetExeCache :: (MonadIO m, MonadReader env m, HasProcessContext env) => m ()
resetExeCache = do
  pc <- view processContextL
  atomicModifyIORef (pcExeCache pc) (const mempty)

-- | Same as 'mkProcessContext' but uses the system environment (from
-- 'System.Environment.getEnvironment').
--
-- @since 0.0.3.0
mkDefaultProcessContext :: MonadIO m => m ProcessContext
mkDefaultProcessContext =
    liftIO $
    getEnvironment >>=
          mkProcessContext
        . Map.fromList . map (T.pack *** T.pack)

-- | Modify the environment variables of a 'ProcessContext'. This will not
-- change the working directory.
--
-- Note that this requires 'MonadIO', as it will create a new 'IORef'
-- for the cache.
--
-- @since 0.0.3.0
modifyEnvVars
  :: MonadIO m
  => ProcessContext
  -> (EnvVars -> EnvVars)
  -> m ProcessContext
modifyEnvVars pc f = do
  pc' <- mkProcessContext (f $ pcTextMap pc)
  return pc' { pcWorkingDir = pcWorkingDir pc }

-- | Use 'modifyEnvVars' to create a new 'ProcessContext', and then
-- use it in the provided action.
--
-- @since 0.0.3.0
withModifyEnvVars
  :: (HasProcessContext env, MonadReader env m, MonadIO m)
  => (EnvVars -> EnvVars)
  -> m a
  -> m a
withModifyEnvVars f inner = do
  pc <- view processContextL
  pc' <- modifyEnvVars pc f
  local (set processContextL pc') inner

-- | Look into the `ProcessContext` and return the specified environmet variable if one is
-- available.
--
-- @since 0.1.14.0
lookupEnvFromContext :: (MonadReader env m, HasProcessContext env) => Text -> m (Maybe Text)
lookupEnvFromContext envName = Map.lookup envName <$> view envVarsL

-- | Set the working directory to be used by child processes.
--
-- @since 0.0.3.0
withWorkingDir
  :: (HasProcessContext env, MonadReader env m, MonadIO m)
  => FilePath
  -> m a
  -> m a
withWorkingDir = local . set workingDirL . Just

-- | Perform pre-call-process tasks.  Ensure the working directory exists and find the
-- executable path.
--
-- Throws a 'ProcessException' if unsuccessful.
--
-- NOT CURRENTLY EXPORTED
preProcess
  :: (HasProcessContext env, MonadReader env m, MonadIO m)
  => String            -- ^ Command name
  -> m FilePath
preProcess name = do
  name' <- findExecutable name >>= either throwIO return
  wd <- view workingDirL
  liftIO $ maybe (return ()) (D.createDirectoryIfMissing True) wd
  return name'

-- | Log running a process with its arguments, for debugging (-v).
--
-- This logs one message before running the process and one message after.
--
-- NOT CURRENTLY EXPORTED
withProcessTimeLog
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Maybe FilePath -- ^ working dirj
  -> String -- ^ executable
  -> [String] -- ^ arguments
  -> m a
  -> m a
withProcessTimeLog mdir name args proc' = do
  let cmdText =
          T.intercalate
              " "
              (T.pack name : map showProcessArgDebug args)
      dirMsg =
        case mdir of
          Nothing -> ""
          Just dir -> " within " <> T.pack dir
  logDebug ("Run process" <> display dirMsg <> ": " <> display cmdText)
  start <- getMonotonicTime
  x <- proc'
  end <- getMonotonicTime
  let diff = end - start
  useColor <- view logFuncUseColorL
  accentColors <- view logFuncAccentColorsL
  logDebug
      ("Process finished in " <>
      (if useColor then accentColors 0 else "") <> -- accent color 0
      timeSpecMilliSecondText diff <>
      (if useColor then "\ESC[0m" else "") <> -- reset
       ": " <> display cmdText)
  return x

timeSpecMilliSecondText :: Double -> Utf8Builder
timeSpecMilliSecondText d = display (round (d * 1000) :: Int) <> "ms"

-- | Provide a 'ProcessConfig' based on the 'ProcessContext' in
-- scope. Deals with resolving the full path, setting the child
-- process's environment variables, setting the working directory, and
-- wrapping the call with 'withProcessTimeLog' for debugging output.
--
-- This is intended to be analogous to the @proc@ function provided by
-- the @System.Process.Typed@ module, but has a different type
-- signature to (1) allow it to perform @IO@ actions for looking up
-- paths, and (2) allow logging and timing of the running action.
--
-- @since 0.0.3.0
proc
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => FilePath -- ^ command to run
  -> [String] -- ^ command line arguments
  -> (ProcessConfig () () () -> m a)
  -> m a
proc name0 args inner = do
  name <- preProcess name0
  wd <- view workingDirL
  envStrings <- view envVarsStringsL

  withProcessTimeLog wd name args
    $ inner
    $ setEnv envStrings
    $ maybe id setWorkingDir wd
    $ P.proc name args

-- | Same as 'P.withProcess', but generalized to 'MonadUnliftIO'.
--
-- @since 0.0.3.0
withProcess
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess pc f = withRunInIO $ \run -> P.withProcessTerm pc (run . f)
{-# DEPRECATED withProcess "Please consider using withProcessWait, or instead use withProcessTerm" #-}

-- | Same as 'P.withProcess_', but generalized to 'MonadUnliftIO'.
--
-- @since 0.0.3.0
withProcess_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess_ pc f = withRunInIO $ \run -> P.withProcessTerm_ pc (run . f)
{-# DEPRECATED withProcess_ "Please consider using withProcessWait, or instead use withProcessTerm" #-}

-- | Same as 'P.withProcessWait', but generalized to 'MonadUnliftIO'.
--
-- @since 0.1.10.0
withProcessWait
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcessWait pc f = withRunInIO $ \run -> P.withProcessWait pc (run . f)

-- | Same as 'P.withProcessWait_', but generalized to 'MonadUnliftIO'.
--
-- @since 0.1.10.0
withProcessWait_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcessWait_ pc f = withRunInIO $ \run -> P.withProcessWait_ pc (run . f)

-- | Same as 'P.withProcessTerm', but generalized to 'MonadUnliftIO'.
--
-- @since 0.1.10.0
withProcessTerm
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcessTerm pc f = withRunInIO $ \run -> P.withProcessTerm pc (run . f)

-- | Same as 'P.withProcessTerm_', but generalized to 'MonadUnliftIO'.
--
-- @since 0.1.10.0
withProcessTerm_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcessTerm_ pc f = withRunInIO $ \run -> P.withProcessTerm_ pc (run . f)

-- | A convenience environment combining a 'LogFunc' and a 'ProcessContext'
--
-- @since 0.0.3.0
data LoggedProcessContext = LoggedProcessContext ProcessContext LogFunc

instance HasLogFunc LoggedProcessContext where
  logFuncL = lens (\(LoggedProcessContext _ lf) -> lf) (\(LoggedProcessContext pc _) lf -> LoggedProcessContext pc lf)
instance HasProcessContext LoggedProcessContext where
  processContextL = lens (\(LoggedProcessContext x _) -> x) (\(LoggedProcessContext _ lf) pc -> LoggedProcessContext pc lf)

-- | Run an action using a 'LoggedProcessContext' with default
-- settings and no logging.
--
-- @since 0.0.3.0
withProcessContextNoLogging :: MonadIO m => RIO LoggedProcessContext a -> m a
withProcessContextNoLogging inner = do
  pc <- mkDefaultProcessContext
  runRIO (LoggedProcessContext pc mempty) inner

-- | Execute a process within the configured environment.
--
-- Execution will not return, because either:
--
-- 1) On non-windows, execution is taken over by execv of the
-- sub-process. This allows signals to be propagated (#527)
--
-- 2) On windows, an 'ExitCode' exception will be thrown.
--
-- @since 0.0.3.0
exec :: (HasProcessContext env, HasLogFunc env) => String -> [String] -> RIO env b
#ifdef WINDOWS
exec = execSpawn
#else
exec cmd0 args = do
    wd <- view workingDirL
    envStringsL <- view envVarsStringsL
    cmd <- preProcess cmd0
    withProcessTimeLog wd cmd args $ liftIO $ do
      for_ wd setCurrentDirectory
      executeFile cmd True args $ Just envStringsL
#endif

-- | Like 'exec', but does not use 'execv' on non-windows. This way,
-- there is a sub-process, which is helpful in some cases
-- (<https://github.com/commercialhaskell/stack/issues/1306>).
--
-- This function only exits by throwing 'ExitCode'.
--
-- @since 0.0.3.0
execSpawn :: (HasProcessContext env, HasLogFunc env) => String -> [String] -> RIO env a
execSpawn cmd args = proc cmd args (runProcess . setStdin inherit) >>= liftIO . exitWith

-- | Check if the given executable exists on the given PATH.
--
-- @since 0.0.3.0
doesExecutableExist
  :: (MonadIO m, MonadReader env m, HasProcessContext env)
  => String            -- ^ Name of executable
  -> m Bool
doesExecutableExist = fmap isRight . findExecutable

-- | Find the complete path for the given executable name.
--
-- On POSIX systems, filenames that match but are not exectuables are excluded.
--
-- On Windows systems, the executable names tried, in turn, are the supplied
-- name (only if it has an extension) and that name extended by each of the
-- 'exeExtensions'. Also, this function may behave differently from
-- 'RIO.Directory.findExecutable'. The latter excludes as executables filenames
-- without a @.bat@, @.cmd@, @.com@ or @.exe@ extension (case-insensitive).
--
-- @since 0.0.3.0
findExecutable
  :: (MonadIO m, MonadReader env m, HasProcessContext env)
  => String
  -- ^ Name of executable
  -> m (Either ProcessException FilePath)
  -- ^ Full path to that executable on success
findExecutable name | any FP.isPathSeparator name = do
  names <- addPcExeExtensions name
  testFPs (pure $ Left $ ExecutableNotFoundAt name) D.makeAbsolute names
findExecutable name = do
  pc <- view processContextL
  m <- readIORef $ pcExeCache pc
  case Map.lookup name m of
    Just epath -> pure epath
    Nothing -> do
      let loop [] = pure $ Left $ ExecutableNotFound name (pcPath pc)
          loop (dir:dirs) = do
            fps <- addPcExeExtensions $ dir FP.</> name
            testFPs (loop dirs) D.makeAbsolute fps
      epath <- loop $ pcPath pc
      () <- atomicModifyIORef (pcExeCache pc) $ \m' ->
          (Map.insert name epath m', ())
      pure epath

-- | A helper function to add the executable extensions of the process context
-- to a file path. On Windows, the original file path is included, if it has an
-- existing extension.
addPcExeExtensions
  :: (MonadIO m, MonadReader env m, HasProcessContext env)
  => FilePath -> m [FilePath]
addPcExeExtensions fp = do
  pc <- view processContextL
  pure $ (if isWindows && FP.hasExtension fp then (fp:) else id)
         (map (fp ++) (pcExeExtensions pc))

-- | A helper function to test whether file paths are to an executable
testFPs
  :: (MonadIO m, MonadReader env m, HasProcessContext env)
  => m (Either ProcessException FilePath)
  -- ^ Default if no executable exists at any file path
  -> (FilePath -> IO FilePath)
  -- ^ Modification to apply to a file path, if an executable exists there
  -> [FilePath]
  -- ^ File paths to test, in turn
  -> m (Either ProcessException FilePath)
testFPs ifNone _ [] = ifNone
testFPs ifNone modify (fp:fps) = do
  exists <- liftIO $ D.doesFileExist fp
  existsExec <- liftIO $ if exists
    then if isWindows then pure True else isExecutable
    else pure False
  if existsExec then liftIO $ Right <$> modify fp else testFPs ifNone modify fps
 where
  isExecutable = D.executable <$> D.getPermissions fp

-- | Get the filename extensions for executable files, including the dot (if
-- any).
--
-- On POSIX systems, this is @[""]@.
--
-- On Windows systems, the list is determined by the value of the @PATHEXT@
-- environment variable, if it present in the environment. If the variable is
-- absent, this is its default value on a Windows system. This function may,
-- therefore, behave differently from 'RIO.Directory.exeExtension',
-- which returns only @".exe"@.
--
-- @since 0.1.13.0
exeExtensions :: (MonadIO m, MonadReader env m, HasProcessContext env)
              => m [String]
exeExtensions = do
  pc <- view processContextL
  return $ pcExeExtensions pc

-- | Augment the given value (assumed to be that of an environment variable
-- that lists paths, such as PATH; this is not checked) with the given extra
-- paths. Those paths are prepended (as in: they take precedence).
--
-- @since 0.0.3.0
augmentPath :: [FilePath] -> Maybe Text -> Either ProcessException Text
augmentPath dirs mpath =
  case filter (FP.searchPathSeparator `elem`) dirs of
    [] -> Right
            $ T.intercalate (T.singleton FP.searchPathSeparator)
            $ map (T.pack . FP.dropTrailingPathSeparator) dirs
            ++ maybeToList mpath
    illegal -> Left $ PathsInvalidInPath illegal

-- | Apply 'augmentPath' on the value of the PATH environment variable in the
-- given 'EnvVars'.
--
-- @since 0.0.3.0
augmentPathMap :: [FilePath] -> EnvVars -> Either ProcessException EnvVars
augmentPathMap = augmentPathMap' "PATH"

-- | Apply 'augmentPath' on the value of the given environment variable in the
-- given 'EnvVars'.
--
-- @since 0.1.22.0
augmentPathMap'
  :: Text
  -- ^ Environment variable. If it does not already exist in the given
  -- 'EnvVars', it will be created.
  -> [FilePath]
  -> EnvVars
  -> Either ProcessException EnvVars
augmentPathMap' envVar dirs (normalizePathEnv -> origEnv) =
  do path <- augmentPath dirs mpath
     return $ Map.insert envVar path origEnv
  where
    mpath = Map.lookup envVar origEnv

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
--
-- @since 0.0.3.0
showProcessArgDebug :: String -> Text
showProcessArgDebug x
    | any special x || null x = T.pack (show x)
    | otherwise = T.pack x
  where special '"' = True
        special ' ' = True
        special _ = False
