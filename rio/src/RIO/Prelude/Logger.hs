{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.Prelude.Logger
  ( -- * Standard logging functions
    logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
    -- * Running with logging
  , withLogFunc
  , LogFunc
  , HasLogFunc (..)
  , logOptionsHandle
    -- ** Log options
  , LogOptions
  , setLogMinLevel
  , setLogVerboseFormat
  , setLogTerminal
  , setLogUseTime
  , setLogUseColor
  , setLogUseLoc
  , updateLogMinLevel
  , updateLogVerboseFormat
    -- * Advanced logging functions
    -- ** Sticky logging
  , logSticky
  , logStickyDone
    -- ** With source
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS
  , logOtherS
    -- ** Generic log function
  , logGeneric
    -- * Advanced running functions
  , mkLogFunc
  , logOptionsMemory
    -- * Data types
  , LogLevel (..)
  , LogSource
  , CallStack
    -- * Convenience functions
  , displayCallStack
    -- * Accessors
  , logFuncUseColorL
  ) where

import RIO.Prelude.Reexports hiding ((<>))
import RIO.Prelude.Renames
import RIO.Prelude.Display
import RIO.Prelude.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Stack (HasCallStack, CallStack, SrcLoc (..), getCallStack, callStack)
import Data.Time
import qualified Data.Text.IO as TIO
import Data.ByteString.Builder (toLazyByteString, char7, byteString, hPutBuilder)
import Data.ByteString.Builder.Extra (flush)
import           GHC.IO.Handle.Internals         (wantWritableHandle)
import           GHC.IO.Encoding.Types           (textEncodingName)
import           GHC.IO.Handle.Types             (Handle__ (..))
import qualified Data.ByteString as B
import           System.IO                  (localeEncoding)
import           GHC.Foreign                (peekCString, withCString)
import Data.Semigroup (Semigroup (..))

-- | The log level of a message.
--
-- @since 0.0.0.0
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther !Text
    deriving (Eq, Show, Read, Ord)

-- | Where in the application a log message came from. Used for
-- display purposes only.
--
-- @since 0.0.0.0
type LogSource = Text

-- | Environment values with a logging function.
--
-- @since 0.0.0.0
class HasLogFunc env where
  logFuncL :: Lens' env LogFunc
instance HasLogFunc LogFunc where
  logFuncL = id

-- | A logging function, wrapped in a newtype for better error messages.
--
-- An implementation may choose any behavior of this value it wishes,
-- including printing to standard output or no action at all.
--
-- @since 0.0.0.0
data LogFunc = LogFunc
  { unLogFunc :: !(CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ())
  , lfOptions :: !(Maybe LogOptions)
  , lfUpdateLogOptions :: !((LogOptions -> LogOptions) -> IO ())
  }

-- | Perform both sets of actions per log entry.
--
-- @since 0.0.0.0
instance Semigroup LogFunc where

  LogFunc f o1 update1 <> LogFunc g o2 update2 = LogFunc
    { unLogFunc = \a b c d -> f a b c d *> g a b c d
    , lfOptions = o1 `mplus` o2
    , lfUpdateLogOptions = \optionMod -> update1 optionMod *> update2 optionMod
    }


-- | 'mempty' peforms no logging.
--
-- @since 0.0.0.0
instance Monoid LogFunc where
  mempty = mkLogFunc $ \_ _ _ _ -> return ()
  mappend = (<>)

-- | Create a 'LogFunc' from the given function.
--
-- @since 0.0.0.0
mkLogFunc :: (CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()) -> LogFunc
mkLogFunc f = LogFunc f Nothing (\_ -> return ())

-- | Generic, basic function for creating other logging functions.
--
-- @since 0.0.0.0
logGeneric
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogLevel
  -> Utf8Builder
  -> m ()
logGeneric src level str = do
  LogFunc {unLogFunc} <- view logFuncL
  liftIO $ unLogFunc callStack src level str

-- | Log a debug level message with no source.
--
-- @since 0.0.0.0
logDebug
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Utf8Builder
  -> m ()
logDebug = logGeneric "" LevelDebug

-- | Log an info level message with no source.
--
-- @since 0.0.0.0
logInfo
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Utf8Builder
  -> m ()
logInfo = logGeneric "" LevelInfo

-- | Log a warn level message with no source.
--
-- @since 0.0.0.0
logWarn
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Utf8Builder
  -> m ()
logWarn = logGeneric "" LevelWarn

-- | Log an error level message with no source.
--
-- @since 0.0.0.0
logError
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Utf8Builder
  -> m ()
logError = logGeneric "" LevelError

-- | Log a message with the specified textual level and no source.
--
-- @since 0.0.0.0
logOther
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Text -- ^ level
  -> Utf8Builder
  -> m ()
logOther = logGeneric "" . LevelOther

-- | Log a debug level message with the given source.
--
-- @since 0.0.0.0
logDebugS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> Utf8Builder
  -> m ()
logDebugS src = logGeneric src LevelDebug

-- | Log an info level message with the given source.
--
-- @since 0.0.0.0
logInfoS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> Utf8Builder
  -> m ()
logInfoS src = logGeneric src LevelInfo

-- | Log a warn level message with the given source.
--
-- @since 0.0.0.0
logWarnS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> Utf8Builder
  -> m ()
logWarnS src = logGeneric src LevelWarn

-- | Log an error level message with the given source.
--
-- @since 0.0.0.0
logErrorS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> Utf8Builder
  -> m ()
logErrorS src = logGeneric src LevelError

-- | Log a message with the specified textual level and the given
-- source.
--
-- @since 0.0.0.0
logOtherS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Text -- ^ level
  -> LogSource
  -> Utf8Builder
  -> m ()
logOtherS src = logGeneric src . LevelOther

-- | Write a "sticky" line to the terminal. Any subsequent lines will
-- overwrite this one, and that same line will be repeated below
-- again. In other words, the line sticks at the bottom of the output
-- forever. Running this function again will replace the sticky line
-- with a new sticky line. When you want to get rid of the sticky
-- line, run 'logStickyDone'.
--
-- Note that not all 'LogFunc' implementations will support sticky
-- messages as described. However, the 'withLogFunc' implementation
-- provided by this module does.
--
-- @since 0.0.0.0
logSticky :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => Utf8Builder -> m ()
logSticky = logOther "sticky"

-- | This will print out the given message with a newline and disable
-- any further stickiness of the line until a new call to 'logSticky'
-- happens.
--
-- @since 0.0.0.0
logStickyDone :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => Utf8Builder -> m ()
logStickyDone = logOther "sticky-done"

-- TODO It might be better at some point to have a 'runSticky' function
-- that encompasses the logSticky->logStickyDone pairing.

canUseUtf8 :: MonadIO m => Handle -> m Bool
canUseUtf8 h = liftIO $ wantWritableHandle "canUseUtf8" h $ \h_ -> do
  -- TODO also handle haOutputNL for CRLF
  return $ (textEncodingName <$> haCodec h_) == Just "UTF-8"

-- | Create a 'LogOptions' value which will store its data in
-- memory. This is primarily intended for testing purposes. This will
-- return both a 'LogOptions' value and an 'IORef' containing the
-- resulting 'Builder' value.
--
-- This will default to non-verbose settings and assume there is a
-- terminal attached. These assumptions can be overridden using the
-- appropriate @set@ functions.
--
-- @since 0.0.0.0
logOptionsMemory :: MonadIO m => m (IORef Builder, LogOptions)
logOptionsMemory = do
  ref <- newIORef mempty
  let options = LogOptions
        { logMinLevel = LevelInfo
        , logVerboseFormat = False
        , logTerminal = True
        , logUseTime = False
        , logUseColor = False
        , logUseLoc = False
        , logSend = \new -> atomicModifyIORef' ref $ \old -> (old <> new, ())
        }
  return (ref, options)

-- | Create a 'LogOptions' value from the given 'Handle' and whether
-- to perform verbose logging or not. Individiual settings can be
-- overridden using appropriate @set@ functions.
--
-- @since 0.0.0.0
logOptionsHandle
  :: MonadIO m
  => Handle
  -> Bool -- ^ verbose?
  -> m LogOptions
logOptionsHandle handle' verbose = liftIO $ do
  terminal <- hIsTerminalDevice handle'
  useUtf8 <- canUseUtf8 handle'
  unicode <- if useUtf8 then return True else getCanUseUnicode
  return LogOptions
    { logMinLevel = if verbose then LevelDebug else LevelInfo
    , logVerboseFormat = verbose
    , logTerminal = terminal
    , logUseTime = verbose
    , logUseColor = verbose && terminal
    , logUseLoc = verbose
    , logSend = \builder ->
        if useUtf8 && unicode
          then hPutBuilder handle' (builder <> flush)
          else do
            let lbs = toLazyByteString builder
                bs = toStrictBytes lbs
            case decodeUtf8' bs of
              Left e -> error $ "mkLogOptions: invalid UTF8 sequence: " ++ show (e, bs)
              Right text -> do
                let text'
                      | unicode = text
                      | otherwise = T.map replaceUnicode text
                TIO.hPutStr handle' text'
                hFlush handle'
    }

-- | Taken from GHC: determine if we should use Unicode syntax
getCanUseUnicode :: IO Bool
getCanUseUnicode = do
    let enc = localeEncoding
        str = "\x2018\x2019"
        test = withCString enc str $ \cstr -> do
            str' <- peekCString enc cstr
            return (str == str')
    test `catchIO` \_ -> return False

updateLogOptions :: IORef LogOptions -> ((LogOptions -> LogOptions) -> IO ())
updateLogOptions loRef = \logModFn ->
  atomicModifyIORef' loRef (\lo -> (logModFn lo, ()))

-- | Given a 'LogOptions' value, run the given function with the
-- specified 'LogFunc'. A common way to use this function is:
--
-- @
-- let isVerbose = False -- get from the command line instead
-- logOptions' <- logOptionsHandle stderr isVerbose
-- let logOptions = setLogUseTime True logOptions'
-- withLogFunc logOptions $ \lf -> do
--   let app = App -- application specific environment
--         { appLogFunc = lf
--         , appOtherStuff = ...
--         }
--   runRIO app $ do
--     logInfo "Starting app"
--     myApp
-- @
--
-- @since 0.0.0.0
withLogFunc :: MonadUnliftIO m => LogOptions -> (LogFunc -> m a) -> m a
withLogFunc options0 inner = withRunInIO $ \run -> do
  optionsRef <- newIORef options0
  if logTerminal options0
    then bracket
            (newMVar mempty)
            (\var -> do
                state <- takeMVar var
                unless (B.null state) (logSend options0 "\n"))
            (\var -> run $ inner $ LogFunc
                { unLogFunc = stickyImpl var optionsRef (simpleLogFunc optionsRef)
                , lfOptions = Just options0
                , lfUpdateLogOptions = updateLogOptions optionsRef
                }
            )
    else
      run $ inner $ LogFunc
        { unLogFunc = \cs src level str ->
             simpleLogFunc optionsRef cs src (noSticky level) str
        , lfOptions = Just options0
        , lfUpdateLogOptions = updateLogOptions optionsRef
        }

-- | Replace Unicode characters with non-Unicode equivalents
replaceUnicode :: Char -> Char
replaceUnicode '\x2018' = '`'
replaceUnicode '\x2019' = '\''
replaceUnicode c = c

noSticky :: LogLevel -> LogLevel
noSticky (LevelOther "sticky-done") = LevelInfo
noSticky (LevelOther "sticky") = LevelInfo
noSticky level = level

-- | Configuration for how to create a 'LogFunc'. Intended to be used
-- with the 'withLogFunc' function.
--
-- @since 0.0.0.0
data LogOptions = LogOptions
  { logMinLevel :: !LogLevel
  , logVerboseFormat :: !Bool
  , logTerminal :: !Bool
  , logUseTime :: !Bool
  , logUseColor :: !Bool
  , logUseLoc :: !Bool
  , logSend :: !(Builder -> IO ())
  }

-- | Set the minimum log level. Messages below this level will not be
-- printed.
--
-- Default: in verbose mode, 'LevelDebug'. Otherwise, 'LevelInfo'.
--
-- @since 0.0.0.0
setLogMinLevel :: LogLevel -> LogOptions -> LogOptions
setLogMinLevel level options = options { logMinLevel = level }

-- | Use the verbose format for printing log messages.
--
-- Default: follows the value of the verbose flag.
--
-- @since 0.0.0.0
setLogVerboseFormat :: Bool -> LogOptions -> LogOptions
setLogVerboseFormat v options = options { logVerboseFormat = v }

-- | Do we treat output as a terminal. If @True@, we will enabled
-- sticky logging functionality.
--
-- Default: checks if the @Handle@ provided to 'logOptionsHandle' is a
-- terminal with 'hIsTerminalDevice'.
--
-- @since 0.0.0.0
setLogTerminal :: Bool -> LogOptions -> LogOptions
setLogTerminal t options = options { logTerminal = t }

-- | Include the time when printing log messages.
--
-- Default: true in debug mode, false otherwise.
--
-- @since 0.0.0.0
setLogUseTime :: Bool -> LogOptions -> LogOptions
setLogUseTime t options = options { logUseTime = t }

-- | Use ANSI color codes in the log output.
--
-- Default: true if in verbose mode /and/ the 'Handle' is a terminal device.
--
-- @since 0.0.0.0
setLogUseColor :: Bool -> LogOptions -> LogOptions
setLogUseColor c options = options { logUseColor = c }

-- | Use code location in the log output.
--
-- Default: true if in verbose mode, false otherwise.
--
-- @since 0.1.2.0
setLogUseLoc :: Bool -> LogOptions -> LogOptions
setLogUseLoc l options = options { logUseLoc = l }

simpleLogFunc :: IORef LogOptions -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
simpleLogFunc loRef cs _src level msg = do
    lo <- readIORef loRef
    when (level >= logMinLevel lo) $ do
      timestamp <- getTimestamp lo
      logSend lo $ getUtf8Builder $
        timestamp <>
        getLevel lo <>
        ansi lo reset <>
        msg <>
        getLoc lo <>
        ansi lo reset <>
        "\n"
  where
   reset = "\ESC[0m"
   setBlack = "\ESC[90m"
   setGreen = "\ESC[32m"
   setBlue = "\ESC[34m"
   setYellow = "\ESC[33m"
   setRed = "\ESC[31m"
   setMagenta = "\ESC[35m"

   ansi :: LogOptions -> Utf8Builder -> Utf8Builder
   ansi lo xs | logUseColor lo = xs
              | otherwise = mempty

   getTimestamp :: LogOptions -> IO Utf8Builder
   getTimestamp lo
     | logVerboseFormat lo && logUseTime lo =
       do now <- getZonedTime
          return $ ansi lo setBlack <> fromString (formatTime' now) <> ": "
     | otherwise = return mempty
     where
       formatTime' =
           take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

   getLevel :: LogOptions -> Utf8Builder
   getLevel lo
     | logVerboseFormat lo =
         case level of
           LevelDebug -> ansi lo setGreen <> "[debug] "
           LevelInfo -> ansi lo setBlue <> "[info] "
           LevelWarn -> ansi lo setYellow <> "[warn] "
           LevelError -> ansi lo setRed <> "[error] "
           LevelOther name ->
             ansi lo setMagenta <>
             "[" <>
             display name <>
             "] "
     | otherwise = mempty

   getLoc :: LogOptions -> Utf8Builder
   getLoc lo
     | logVerboseFormat lo = ansi lo setBlack <> "\n@(" <> displayCallStack cs <> ")"
     | otherwise = mempty

-- | Convert a 'CallStack' value into a 'Utf8Builder' indicating
-- the first source location.
--
-- TODO Consider showing the entire call stack instead.
--
-- @since 0.0.0.0
displayCallStack :: CallStack -> Utf8Builder
displayCallStack cs =
     case reverse $ getCallStack cs of
       [] -> "<no call stack found>"
       (_desc, loc):_ ->
         let file = srcLocFile loc
          in fromString file <>
             ":" <>
             displayShow (srcLocStartLine loc) <>
             ":" <>
             displayShow (srcLocStartCol loc)

-- | The length of a timestamp in the format "YYYY-MM-DD hh:mm:ss.μμμμμμ".
-- This definition is top-level in order to avoid multiple reevaluation at runtime.
timestampLength :: Int
timestampLength =
  length (formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0))

-- | Allows to update the 'LogLevel' of an existing 'LogFunc' record
--
-- @since 0.1.2.0
updateLogMinLevel :: MonadIO m => LogFunc -> LogLevel -> m ()
updateLogMinLevel (LogFunc {lfUpdateLogOptions}) level =
  liftIO $ lfUpdateLogOptions (setLogMinLevel level)

-- | Allows to update the verbose format of an existing 'LogFunc' record
--
-- @since 0.1.2.0
updateLogVerboseFormat :: MonadIO m => LogFunc -> Bool -> m ()
updateLogVerboseFormat (LogFunc {lfUpdateLogOptions}) isVerbose =
  liftIO $ lfUpdateLogOptions (setLogVerboseFormat isVerbose)

stickyImpl
    :: MVar ByteString -> IORef LogOptions
    -> (CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ())
    -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
stickyImpl ref loRef logFunc loc src level msgOrig = modifyMVar_ ref $ \sticky -> do
  lo <- readIORef loRef
  let backSpaceChar = '\8'
      repeating = mconcat . replicate (B.length sticky) . char7
      clear = logSend lo
        (repeating backSpaceChar <>
        repeating ' ' <>
        repeating backSpaceChar)

  case level of
    LevelOther "sticky-done" -> do
      clear
      logFunc loc src LevelInfo msgOrig
      return mempty
    LevelOther "sticky" -> do
      clear
      let bs = toStrictBytes $ toLazyByteString $ getUtf8Builder msgOrig
      logSend lo (byteString bs <> flush)
      return bs
    _
      | level >= logMinLevel lo -> do
          clear
          logFunc loc src level msgOrig
          unless (B.null sticky) $ logSend lo (byteString sticky <> flush)
          return sticky
      | otherwise -> return sticky

-- | Is the log func configured to use color output?
--
-- Intended for use by code which wants to optionally add additional color to
-- its log messages.
--
-- @since 0.1.0.0
logFuncUseColorL :: HasLogFunc env => SimpleGetter env Bool
logFuncUseColorL = logFuncL.to (maybe False logUseColor . lfOptions)
