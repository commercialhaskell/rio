{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
module RIO.Prelude.Logger
  ( -- ** Running with logging
    withLogFunc
  , newLogFunc
  , LogFunc
  , HasLogFunc (..)
  , logOptionsHandle
    -- *** Log options
  , LogOptions
  , setLogMinLevel
  , setLogMinLevelIO
  , setLogVerboseFormat
  , setLogVerboseFormatIO
  , setLogTerminal
  , setLogUseTime
  , setLogUseColor
  , setLogUseLoc
  , LogFormat
  , setLogFormat
  , composeLogFormat
  , setLogLevelColors
  , setLogSecondaryColor
  , setLogAccentColors
    -- ** Standard logging functions
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
    -- ** Advanced logging functions
    -- *** Sticky logging
  , logSticky
  , logStickyDone
    -- *** With source
    --
    -- $withSource
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS
  , logOtherS
    -- *** Generic log function
  , logGeneric
    -- ** Advanced running functions
  , mkLogFunc
  , logOptionsMemory
    -- ** Data types
  , LogLevel (..)
  , LogSource
  , CallStack
    -- ** Convenience functions
  , displayCallStack
  , noLogging
    -- ** Accessors
  , logFuncUseColorL
  , logFuncLogLevelColorsL
  , logFuncSecondaryColorL
  , logFuncAccentColorsL
    -- * Type-generic logger
    -- $type-generic-intro
  , glog
  , GLogFunc
  , gLogFuncClassic
  , mkGLogFunc
  , contramapMaybeGLogFunc
  , contramapGLogFunc
  , HasGLogFunc(..)
  , HasLogLevel(..)
  , HasLogSource(..)
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
import Data.Bits
import Data.ByteString.Builder (toLazyByteString, char7, byteString, hPutBuilder)
import Data.ByteString.Builder.Extra (flush)
import           GHC.IO.Encoding.Types           (textEncodingName)
import qualified Data.ByteString as B
import           System.IO                  (hGetEncoding, localeEncoding)
import           System.IO.Error            (illegalOperationErrorType,
                                             mkIOError, ioError)
import           GHC.Foreign                (peekCString, withCString)
import Data.Semigroup (Semigroup (..))

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif

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
  }

-- | Perform both sets of actions per log entry.
--
-- @since 0.0.0.0
instance Semigroup LogFunc where
  LogFunc f o1 <> LogFunc g o2 = LogFunc
    { unLogFunc = \a b c d -> f a b c d *> g a b c d
    , lfOptions = o1 `mplus` o2
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
mkLogFunc f = LogFunc f Nothing

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
  LogFunc logFunc _ <- view logFuncL
  liftIO $ logFunc callStack src level str

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

-- $withSource
--
-- There is a set of logging functions that take an extra 'LogSource'
-- argument to provide context, typically detailing what part of an
-- application the message comes from.
--
-- For example, in verbose mode, @infoLogS "database" "connected"@ will
-- result in
--
-- > [info] (database) connected

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
canUseUtf8 h = liftIO $ do
  isWritable <- hIsWritable h
  unless isWritable $ ioError $ mkIOError illegalOperationErrorType
                                          "canUseUtf8"
                                          (Just h)
                                          Nothing
  maybeEnc <- hGetEncoding h
  return $ fmap textEncodingName maybeEnc == Just "UTF-8"

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
        { logMinLevel = return LevelInfo
        , logVerboseFormat = return False
        , logTerminal = True
        , logUseTime = False
        , logUseColor = False
        , logColors = defaultLogColors
        , logUseLoc = False
        , logFormat = id
        , logSend = \new -> atomicModifyIORef' ref $ \old -> (old <> new, ())
        }
  return (ref, options)

-- | Create a 'LogOptions' value from the given 'Handle' and whether
-- to perform verbose logging or not. Individiual settings can be
-- overridden using appropriate @set@ functions.
-- Logging output is guaranteed to be non-interleaved only for a
-- UTF-8 'Handle' in a multi-thread environment.
--
-- When Verbose Flag is @True@, the following happens:
--
--     * @setLogVerboseFormat@ is called with @True@
--     * @setLogUseColor@ is called with @True@ (except on Windows)
--     * @setLogUseLoc@ is called with @True@
--     * @setLogUseTime@ is called with @True@
--     * @setLogMinLevel@ is called with 'Debug' log level
--
-- @since 0.0.0.0
logOptionsHandle
  :: MonadIO m
  => Handle
  -> Bool -- ^ Verbose Flag
  -> m LogOptions
logOptionsHandle handle' verbose = liftIO $ do
  terminal <- hIsTerminalDevice handle'
  useUtf8 <- canUseUtf8 handle'
  unicode <- if useUtf8 then return True else getCanUseUnicode
  return LogOptions
    { logMinLevel = return $ if verbose then LevelDebug else LevelInfo
    , logVerboseFormat = return verbose
    , logTerminal = terminal
    , logUseTime = verbose
#if WINDOWS
    , logUseColor = False
#else
    , logUseColor = verbose && terminal
#endif
    , logColors = defaultLogColors
    , logUseLoc = verbose
    , logFormat = id
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


-- | Given a 'LogOptions' value, returns both a new 'LogFunc' and a sub-routine that
-- disposes it.
--
-- Intended for use if you want to deal with the teardown of 'LogFunc' yourself,
-- otherwise prefer the 'withLogFunc' function instead.
--
--  @since 0.1.3.0
newLogFunc :: (MonadIO n, MonadIO m) => LogOptions -> n (LogFunc, m ())
newLogFunc options =
  if logTerminal options then do
    var <- newMVar (mempty,0)
    return (LogFunc
             { unLogFunc = stickyImpl var options (simpleLogFunc options)
             , lfOptions = Just options
             }
           , do (state,_) <- takeMVar var
                unless (B.null state) (liftIO $ logSend options "\n")
           )
  else
    return (LogFunc
            { unLogFunc = \cs src level str ->
                simpleLogFunc options cs src (noSticky level) str
            , lfOptions = Just options
            }
           , return ()
           )

-- | Given a 'LogOptions' value, run the given function with the
-- specified 'LogFunc'. A common way to use this function is:
--
-- @
-- let isVerbose = False -- get from the command line instead
-- logOptions' <- logOptionsHandle stderr isVerbose
-- let logOptions = setLogUseTime True logOptions'
-- withLogFunc logOptions $ \\lf -> do
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
withLogFunc options inner = withRunInIO $ \run -> do
  bracket (newLogFunc options)
          snd
          (run . inner . fst)


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
  { logMinLevel :: !(IO LogLevel)
  , logVerboseFormat :: !(IO Bool)
  , logTerminal :: !Bool
  , logUseTime :: !Bool
  , logUseColor :: !Bool
  , logColors :: !LogColors
  , logUseLoc :: !Bool
  , logFormat :: !LogFormat
  , logSend :: !(Builder -> IO ())
  }

-- | ANSI color codes for use in the configuration of the creation of a
-- 'LogFunc'.
--
-- @since 0.1.18.0
data LogColors = LogColors
  { -- | The color associated with each 'LogLevel'.
    logColorLogLevels :: !(LogLevel -> Utf8Builder)
    -- | The color of secondary content.
  , logColorSecondary :: !Utf8Builder
    -- | The color of accents, which are indexed by 'Int'.
  , logColorAccents :: !(Int -> Utf8Builder)
  }

defaultLogColors :: LogColors
defaultLogColors = LogColors
  { logColorLogLevels = defaultLogLevelColors
  , logColorSecondary = defaultLogSecondaryColor
  , logColorAccents = defaultLogAccentColors
  }

defaultLogLevelColors :: LogLevel -> Utf8Builder
defaultLogLevelColors LevelDebug = "\ESC[32m" -- Green
defaultLogLevelColors LevelInfo = "\ESC[34m" -- Blue
defaultLogLevelColors LevelWarn = "\ESC[33m" -- Yellow
defaultLogLevelColors LevelError = "\ESC[31m" -- Red
defaultLogLevelColors (LevelOther _) = "\ESC[35m" -- Magenta

defaultLogSecondaryColor :: Utf8Builder
defaultLogSecondaryColor = "\ESC[90m"  -- Bright black (gray)

defaultLogAccentColors :: Int -> Utf8Builder
defaultLogAccentColors = const "\ESC[92m" -- Bright green

-- | Set the minimum log level. Messages below this level will not be
-- printed.
--
-- Default: in verbose mode, 'LevelDebug'. Otherwise, 'LevelInfo'.
--
-- @since 0.0.0.0
setLogMinLevel :: LogLevel -> LogOptions -> LogOptions
setLogMinLevel level options = options { logMinLevel = return level }

-- | Refer to 'setLogMinLevel'. This modifier allows to alter the verbose format
-- value dynamically at runtime.
--
-- Default: in verbose mode, 'LevelDebug'. Otherwise, 'LevelInfo'.
--
-- @since 0.1.3.0
setLogMinLevelIO :: IO LogLevel -> LogOptions -> LogOptions
setLogMinLevelIO getLevel options = options { logMinLevel = getLevel }

-- | Use the verbose format for printing log messages.
--
-- Default: follows the value of the verbose flag.
--
-- @since 0.0.0.0
setLogVerboseFormat :: Bool -> LogOptions -> LogOptions
setLogVerboseFormat v options = options { logVerboseFormat = return v }

-- | Refer to 'setLogVerboseFormat'. This modifier allows to alter the verbose
--   format value dynamically at runtime.
--
-- Default: follows the value of the verbose flag.
--
-- @since 0.1.3.0
setLogVerboseFormatIO :: IO Bool -> LogOptions -> LogOptions
setLogVerboseFormatIO getVerboseLevel options =
  options { logVerboseFormat = getVerboseLevel }

-- | Do we treat output as a terminal. If @True@, we will enable
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
-- Default: `True` in debug mode, `False` otherwise.
--
-- @since 0.0.0.0
setLogUseTime :: Bool -> LogOptions -> LogOptions
setLogUseTime t options = options { logUseTime = t }

-- | Use ANSI color codes in the log output.
--
-- Default: `True` if in verbose mode /and/ the 'Handle' is a terminal device.
--
-- @since 0.0.0.0
setLogUseColor :: Bool -> LogOptions -> LogOptions
setLogUseColor c options = options { logUseColor = c }

-- | ANSI color codes for 'LogLevel' in the log output.
--
-- Default: 'LevelDebug'   = \"\\ESC[32m\" -- Green
--          'LevelInfo'    = \"\\ESC[34m\" -- Blue
--          'LevelWarn'    = \"\\ESC[33m\" -- Yellow
--          'LevelError'   = \"\\ESC[31m\" -- Red
--          'LevelOther' _ = \"\\ESC[35m\" -- Magenta
--
-- @since 0.1.18.0
setLogLevelColors :: (LogLevel -> Utf8Builder) -> LogOptions -> LogOptions
setLogLevelColors logLevelColors options =
  let lc = (logColors options){ logColorLogLevels = logLevelColors }
  in  options { logColors = lc }

-- | ANSI color codes for secondary content in the log output.
--
-- Default: \"\\ESC[90m\" -- Bright black (gray)
--
-- @since 0.1.18.0
setLogSecondaryColor :: Utf8Builder -> LogOptions -> LogOptions
setLogSecondaryColor c options =
  let lc = (logColors options){ logColorSecondary = c }
  in  options { logColors = lc }

-- | ANSI color codes for accents in the log output. Accent colors are indexed
-- by 'Int'.
--
-- Default: 'const' \"\\ESC[92m\" -- Bright green, for all indicies
--
-- @since 0.1.18.0
setLogAccentColors
  :: (Int -> Utf8Builder)  -- ^ This should be a total function.
  -> LogOptions
  -> LogOptions
setLogAccentColors accentColors options =
  let lc = (logColors options){ logColorAccents = accentColors }
  in  options { logColors = lc }

-- | Use code location in the log output.
--
-- Default: `True` if in verbose mode, `False` otherwise.
--
-- @since 0.1.2.0
setLogUseLoc :: Bool -> LogOptions -> LogOptions
setLogUseLoc l options = options { logUseLoc = l }

-- | Format method for messages
--
-- @since 0.1.24.0
type LogFormat = Utf8Builder -> Utf8Builder

-- | Set format method for messages
--
-- Default: `id`
--
-- @since 0.1.13.0
setLogFormat :: LogFormat -> LogOptions -> LogOptions
setLogFormat f options = options { logFormat = f }

-- | Modify existing format method
--
-- @since 0.1.24.0
composeLogFormat :: (LogFormat -> LogFormat) -> LogOptions -> LogOptions
composeLogFormat f' options = setLogFormat (f' $ logFormat options) options

simpleLogFunc :: LogOptions -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
simpleLogFunc lo cs src level msg = do
    logLevel   <- logMinLevel lo
    logVerbose <- logVerboseFormat lo

    when (level >= logLevel) $ do
      timestamp <- getTimestamp logVerbose
      logSend lo $ getUtf8Builder $
        timestamp <>
        getLevel logVerbose <>
        ansi reset <>
        getSource <>
        logFormat lo msg <>
        getLoc <>
        ansi reset <>
        "\n"
  where
   reset = "\ESC[0m"
   lc = logColors lo
   levelColor = logColorLogLevels lc level
   timestampColor = logColorSecondary lc
   locColor = logColorSecondary lc

   ansi :: Utf8Builder -> Utf8Builder
   ansi xs | logUseColor lo = xs
           | otherwise = mempty

   getTimestamp :: Bool -> IO Utf8Builder
   getTimestamp logVerbose
     | logVerbose && logUseTime lo =
       do now <- getZonedTime
          return $ ansi timestampColor <> fromString (formatTime' now) <> ": "
     | otherwise = return mempty
     where
       formatTime' =
           take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

   getLevel :: Bool -> Utf8Builder
   getLevel logVerbose
     | logVerbose = ansi levelColor <>
         case level of
           LevelDebug -> "[debug] "
           LevelInfo -> "[info] "
           LevelWarn -> "[warn] "
           LevelError -> "[error] "
           LevelOther name ->
             "[" <>
             display name <>
             "] "
     | otherwise = mempty

   getSource :: Utf8Builder
   getSource = case src of
     "" -> ""
     _  -> "(" <> display src <> ") "

   getLoc :: Utf8Builder
   getLoc
     | logUseLoc lo = ansi locColor <> "\n@(" <> displayCallStack cs <> ")"
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

stickyImpl
    :: MVar (ByteString,Int) -> LogOptions
    -> (CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ())
    -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
stickyImpl ref lo logFunc loc src level msgOrig = modifyMVar_ ref $ \(sticky,stickyLen) -> do
  let backSpaceChar = '\8'
      repeating = mconcat . replicate stickyLen . char7
      clear = logSend lo
        (repeating backSpaceChar <>
        repeating ' ' <>
        repeating backSpaceChar)

  logLevel <- logMinLevel lo

  case level of
    LevelOther "sticky-done" -> do
      clear
      logFunc loc src LevelInfo msgOrig
      return (mempty,0)
    LevelOther "sticky" -> do
      clear
      let bs = toStrictBytes $ toLazyByteString $ getUtf8Builder msgOrig
      logSend lo (byteString bs <> flush)
      return (bs, utf8CharacterCount bs)
    _
      | level >= logLevel -> do
          clear
          logFunc loc src level msgOrig
          unless (B.null sticky) $ logSend lo (byteString sticky <> flush)
          return (sticky,stickyLen)
      | otherwise -> return (sticky,stickyLen)

-- | The number of Unicode characters in a UTF-8 encoded byte string,
-- excluding ANSI CSI sequences.
utf8CharacterCount :: ByteString -> Int
utf8CharacterCount = go 0
  where
    go !n bs = case B.uncons bs of
        Nothing -> n
        Just (c,bs)
            | c .&. 0xC0 == 0x80 -> go n bs            -- UTF-8 continuation
            | c == 0x1B          -> go n $ dropCSI bs  -- ANSI escape
            | otherwise          -> go (n+1) bs

    dropCSI bs = case B.uncons bs of
        Just (0x5B,bs2) -> B.drop 1 $ B.dropWhile isSequenceByte bs2
        _               -> bs

    isSequenceByte c = c >= 0x20 && c <= 0x3F

-- | Is the log func configured to use color output?
--
-- Intended for use by code which wants to optionally add additional color to
-- its log messages.
--
-- @since 0.1.0.0
logFuncUseColorL :: HasLogFunc env => SimpleGetter env Bool
logFuncUseColorL = logFuncL.to (maybe False logUseColor . lfOptions)

-- | What color is the log func configured to use for each 'LogLevel'?
--
-- Intended for use by code which wants to optionally add additional color to
-- its log messages.
--
-- @since 0.1.18.0
logFuncLogLevelColorsL :: HasLogFunc env
                       => SimpleGetter env (LogLevel -> Utf8Builder)
logFuncLogLevelColorsL = logFuncL.to
                           (maybe defaultLogLevelColors
                                  (logColorLogLevels . logColors) . lfOptions)

-- | What color is the log func configured to use for secondary content?
--
-- Intended for use by code which wants to optionally add additional color to
-- its log messages.
--
-- @since 0.1.18.0
logFuncSecondaryColorL :: HasLogFunc env
                       => SimpleGetter env Utf8Builder
logFuncSecondaryColorL = logFuncL.to
                           (maybe defaultLogSecondaryColor
                                  (logColorSecondary . logColors) . lfOptions)

-- | What accent colors, indexed by 'Int', is the log func configured to use?
--
-- Intended for use by code which wants to optionally add additional color to
-- its log messages.
--
-- @since 0.1.18.0
logFuncAccentColorsL :: HasLogFunc env
                       => SimpleGetter env (Int -> Utf8Builder)
logFuncAccentColorsL = logFuncL.to
                           (maybe defaultLogAccentColors
                                  (logColorAccents . logColors) . lfOptions)

-- | Disable logging capabilities in a given sub-routine
--
-- Intended to skip logging in general purpose implementations, where secrets
-- might be logged accidently.
--
-- @since 0.1.5.0
noLogging :: (HasLogFunc env, MonadReader env m) => m a -> m a
noLogging = local (set logFuncL mempty)

--------------------------------------------------------------------------------
--
-- $type-generic-intro
--
-- When logging takes on a more semantic meaning and the logs need to
-- be digested, acted upon, translated or serialized upstream (to
-- e.g. a JSON logging server), we have 'GLogFunc' (as in "generic log
-- function"), and is accessed via 'HasGLogFunc'.
--
-- There is only one function to log in this system: the 'glog'
-- function, which can log any message. You determine the log levels
-- or severity of messages when needed.
--
-- Using 'RIO.Prelude.mapRIO' and 'contramapGLogFunc' (or
-- 'contramapMaybeGLogFunc'), you can build hierarchies of loggers.
--
-- Example:
--
-- @
-- import RIO
--
-- data DatabaseMsg = Connected String | Query String | Disconnected deriving Show
-- data WebMsg = Request String | Error String | DatabaseMsg DatabaseMsg deriving Show
-- data AppMsg = InitMsg String | WebMsg WebMsg deriving Show
--
-- main :: IO ()
-- main =
--   runRIO
--     (mkGLogFunc (\stack msg -> print msg))
--     (do glog (InitMsg "Ready to go!")
--         runWeb
--           (do glog (Request "/foo")
--               runDB (do glog (Connected "127.0.0.1")
--                         glog (Query "SELECT 1"))
--               glog (Error "Oh noes!")))
--
-- runDB :: RIO (GLogFunc DatabaseMsg) () -> RIO (GLogFunc WebMsg) ()
-- runDB = mapRIO (contramapGLogFunc DatabaseMsg)
--
-- runWeb :: RIO (GLogFunc WebMsg) () -> RIO (GLogFunc AppMsg) ()
-- runWeb = mapRIO (contramapGLogFunc WebMsg)
-- @
--
-- If we instead decided that we only wanted to log database queries,
-- and not bother the upstream with connect/disconnect messages, we
-- could simplify the constructor to @DatabaseQuery String@:
--
-- @
-- data WebMsg = Request String | Error String | DatabaseQuery String deriving Show
-- @
--
-- And then @runDB@ could use 'contramapMaybeGLogFunc' to parse only queries:
--
-- @
-- runDB =
--   mapRIO
--     (contramapMaybeGLogFunc
--        (\msg ->
--           case msg of
--             Query string -> pure (DatabaseQuery string)
--             _ -> Nothing))
-- @
--
-- This way, upstream only has to care about queries and not
-- connect/disconnect constructors.

-- | An app is capable of generic logging if it implements this.
--
-- @since 0.1.13.0
class HasGLogFunc env where
  type GMsg env
  gLogFuncL :: Lens' env (GLogFunc (GMsg env))

-- | Quick way to run a RIO that only has a logger in its environment.
--
-- @since 0.1.13.0
instance HasGLogFunc (GLogFunc msg) where
  type GMsg (GLogFunc msg) = msg
  gLogFuncL = id

-- | A generic logger of some type @msg@.
--
-- Your 'GLocFunc' can re-use the existing classical logging framework
-- of RIO, and/or implement additional transforms,
-- filters. Alternatively, you may log to a JSON source in a database,
-- or anywhere else as needed. You can decide how to log levels or
-- severities based on the constructors in your type. You will
-- normally determine this in your main app entry point.
--
-- @since 0.1.13.0
newtype GLogFunc msg = GLogFunc (CallStack -> msg -> IO ())

#if MIN_VERSION_base(4,12,0)
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Contravariant.html

-- | Use this instance to wrap sub-loggers via 'RIO.mapRIO'.
--
-- The 'Contravariant' class is available in base 4.12.0.
--
-- @since 0.1.13.0
instance Contravariant GLogFunc where
  contramap = contramapGLogFunc
  {-# INLINABLE contramap #-}
#endif

-- | Perform both sets of actions per log entry.
--
-- @since 0.1.13.0
instance Semigroup (GLogFunc msg) where
  GLogFunc f <> GLogFunc g = GLogFunc (\a b -> f a b *> g a b)

-- | 'mempty' peforms no logging.
--
-- @since 0.1.13.0
instance Monoid (GLogFunc msg) where
  mempty = mkGLogFunc $ \_ _ -> return ()
  mappend = (<>)

-- | A vesion of 'contramapMaybeGLogFunc' which supports filering.
--
-- @since 0.1.13.0
contramapMaybeGLogFunc :: (a -> Maybe b) -> GLogFunc b -> GLogFunc a
contramapMaybeGLogFunc f (GLogFunc io) =
  GLogFunc (\stack msg -> maybe (pure ()) (io stack) (f msg))
{-# INLINABLE contramapMaybeGLogFunc #-}

-- | A contramap. Use this to wrap sub-loggers via 'RIO.mapRIO'.
--
-- If you are on base > 4.12.0, you can just use 'contramap'.
--
-- @since 0.1.13.0
contramapGLogFunc :: (a -> b) -> GLogFunc b -> GLogFunc a
contramapGLogFunc f (GLogFunc io) = GLogFunc (\stack msg -> io stack (f msg))
{-# INLINABLE contramapGLogFunc #-}

-- | Make a custom generic logger. With this you could, for example,
-- write to a database or a log digestion service. For example:
--
-- > mkGLogFunc (\stack msg -> send (Data.Aeson.encode (JsonLog stack msg)))
--
-- @since 0.1.13.0
mkGLogFunc :: (CallStack -> msg -> IO ()) -> GLogFunc msg
mkGLogFunc = GLogFunc

-- | Log a value generically.
--
-- @since 0.1.13.0
glog ::
     (MonadIO m, HasCallStack, HasGLogFunc env, MonadReader env m)
  => GMsg env
  -> m ()
glog t = do
  GLogFunc gLogFunc <- view gLogFuncL
  liftIO (gLogFunc callStack t)
{-# INLINABLE glog #-}

--------------------------------------------------------------------------------
-- Integration with classical logger framework

-- | Level, if any, of your logs. If unknown, use 'LogOther'. Use for
-- your generic log data types that want to sit inside the classic log
-- framework.
--
-- @since 0.1.13.0
class HasLogLevel msg where
  getLogLevel :: msg -> LogLevel

-- | Source of a log. This can be whatever you want. Use for your
-- generic log data types that want to sit inside the classic log
-- framework.
--
-- @since 0.1.13.0
class HasLogSource msg where
  getLogSource :: msg -> LogSource

-- | Make a 'GLogFunc' via classic 'LogFunc'. Use this if you'd like
-- to log your generic data type via the classic RIO terminal logger.
--
-- @since 0.1.13.0
gLogFuncClassic ::
     (HasLogLevel msg, HasLogSource msg, Display msg) => LogFunc -> GLogFunc msg
gLogFuncClassic (LogFunc {unLogFunc = io}) =
  mkGLogFunc
    (\theCallStack msg ->
       liftIO
         (io theCallStack (getLogSource msg) (getLogLevel msg) (display msg)))
