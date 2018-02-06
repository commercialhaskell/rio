{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module RIO.Logger
  ( LogLevel (..)
  , LogSource
  , LogStr
  , LogFunc
  , CallStack
  , HasLogFunc (..)
  , logGeneric
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS
  , logOther
  , logSticky
  , logStickyDone
  , runNoLogging
  , NoLogging (..)
  , withStickyLogger
  , LogOptions (..)
  , displayCallStack
  , mkLogOptions
  ) where

import RIO.Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Lens.Micro (to)
import GHC.Stack (HasCallStack, CallStack, SrcLoc (..), getCallStack, callStack)
import Data.Time
import qualified Data.Text.IO as TIO
import Data.ByteString.Builder (toLazyByteString, char7, byteString)
import Data.ByteString.Builder.Extra (flush)
import           GHC.IO.Handle.Internals         (wantWritableHandle)
import           GHC.IO.Encoding.Types           (textEncodingName)
import           GHC.IO.Handle.Types             (Handle__ (..))
import qualified Data.ByteString as B
import           System.IO                  (localeEncoding)
import           GHC.Foreign                (peekCString, withCString)

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther !Text
    deriving (Eq, Show, Read, Ord)

type LogSource = Text
type LogStr = DisplayBuilder
class HasLogFunc env where
  logFuncL :: SimpleGetter env LogFunc
instance HasLogFunc LogFunc where
  logFuncL = id

type LogFunc = CallStack -> LogSource -> LogLevel -> LogStr -> IO ()

logGeneric
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogLevel
  -> LogStr
  -> m ()
logGeneric src level str = do
  logFunc <- view logFuncL
  liftIO $ logFunc callStack src level str

logDebug
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logDebug = logGeneric "" LevelDebug

logInfo
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logInfo = logGeneric "" LevelInfo

logWarn
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logWarn = logGeneric "" LevelWarn

logError
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logError = logGeneric "" LevelError

logDebugS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogStr
  -> m ()
logDebugS src = logGeneric src LevelDebug

logInfoS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogStr
  -> m ()
logInfoS src = logGeneric src LevelInfo

logWarnS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogStr
  -> m ()
logWarnS src = logGeneric src LevelWarn

logErrorS
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogStr
  -> m ()
logErrorS src = logGeneric src LevelError

logOther
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Text -- ^ level
  -> LogStr
  -> m ()
logOther = logGeneric "" . LevelOther

runNoLogging :: MonadIO m => ReaderT NoLogging m a -> m a
runNoLogging = flip runReaderT NoLogging

data NoLogging = NoLogging
instance HasLogFunc NoLogging where
  logFuncL = to (\_ _ _ _ _ -> return ())

-- | Write a "sticky" line to the terminal. Any subsequent lines will
-- overwrite this one, and that same line will be repeated below
-- again. In other words, the line sticks at the bottom of the output
-- forever. Running this function again will replace the sticky line
-- with a new sticky line. When you want to get rid of the sticky
-- line, run 'logStickyDone'.
--
logSticky :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => LogStr -> m ()
logSticky = logOther "sticky"

-- | This will print out the given message with a newline and disable
-- any further stickiness of the line until a new call to 'logSticky'
-- happens.
--
-- It might be better at some point to have a 'runSticky' function
-- that encompasses the logSticky->logStickyDone pairing.
logStickyDone :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => LogStr -> m ()
logStickyDone = logOther "sticky-done"

canUseUtf8 :: MonadIO m => Handle -> m Bool
canUseUtf8 h = liftIO $ wantWritableHandle "canUseUtf8" h $ \h_ -> do
  -- TODO also handle haOutputNL for CRLF
  return $ (textEncodingName <$> haCodec h_) == Just "UTF-8"

mkLogOptions
  :: MonadIO m
  => Handle
  -> Bool -- ^ verbose?
  -> m LogOptions
mkLogOptions handle verbose = liftIO $ do
  terminal <- hIsTerminalDevice handle
  useUtf8 <- canUseUtf8 handle
  unicode <- if useUtf8 then return True else getCanUseUnicode
  return LogOptions
    { logMinLevel = if verbose then LevelDebug else LevelInfo
    , logVerboseFormat = verbose
    , logTerminal = terminal
    , logUseTime = verbose
    , logUseColor = verbose
    , logSend = \builder ->
        if useUtf8 && unicode
          then hPutBuilder handle (builder <> flush)
          else do
            let lbs = toLazyByteString builder
                bs = toStrictBytes lbs
            case decodeUtf8' bs of
              Left e -> error $ "mkLogOptions: invalid UTF8 sequence: " ++ show (e, bs)
              Right text -> do
                let text'
                      | unicode = text
                      | otherwise = T.map replaceUnicode text
                TIO.hPutStr handle text'
                hFlush handle
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

withStickyLogger :: MonadUnliftIO m => LogOptions -> (LogFunc -> m a) -> m a
withStickyLogger options inner = withRunInIO $ \run -> do
  if logTerminal options
    then withSticky options $ \var ->
           run $ inner $ stickyImpl var options (simpleLogFunc options)
    else
      run $ inner $ \cs src level str ->
      simpleLogFunc options cs src (noSticky level) str

-- | Replace Unicode characters with non-Unicode equivalents
replaceUnicode :: Char -> Char
replaceUnicode '\x2018' = '`'
replaceUnicode '\x2019' = '\''
replaceUnicode c = c

noSticky :: LogLevel -> LogLevel
noSticky (LevelOther "sticky-done") = LevelInfo
noSticky (LevelOther "sticky") = LevelInfo
noSticky level = level

data LogOptions = LogOptions
  { logMinLevel :: !LogLevel
  , logVerboseFormat :: !Bool
  , logTerminal :: !Bool
  , logUseTime :: !Bool
  , logUseColor :: !Bool
  , logSend :: !(Builder -> IO ())
  }

simpleLogFunc :: LogOptions -> LogFunc
simpleLogFunc lo cs _src level msg =
    when (level >= logMinLevel lo) $ do
      timestamp <- getTimestamp
      logSend lo $ getUtf8Builder $
        timestamp <>
        getLevel <>
        ansi reset <>
        msg <>
        getLoc <>
        ansi reset <>
        "\n"
  where
   reset = "\ESC[0m"
   setBlack = "\ESC[90m"
   setGreen = "\ESC[32m"
   setBlue = "\ESC[34m"
   setYellow = "\ESC[33m"
   setRed = "\ESC[31m"
   setMagenta = "\ESC[35m"

   ansi :: DisplayBuilder -> DisplayBuilder
   ansi xs | logUseColor lo = xs
           | otherwise = mempty

   getTimestamp :: IO DisplayBuilder
   getTimestamp
     | logVerboseFormat lo && logUseTime lo =
       do now <- getZonedTime
          return $ ansi setBlack <> fromString (formatTime' now) <> ": "
     | otherwise = return mempty
     where
       formatTime' =
           take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

   getLevel :: DisplayBuilder
   getLevel
     | logVerboseFormat lo =
         case level of
           LevelDebug -> ansi setGreen <> "[debug] "
           LevelInfo -> ansi setBlue <> "[info] "
           LevelWarn -> ansi setYellow <> "[warn] "
           LevelError -> ansi setRed <> "[error] "
           LevelOther name ->
             ansi setMagenta <>
             "[" <>
             display name <>
             "] "
     | otherwise = mempty

   getLoc :: DisplayBuilder
   getLoc
     | logVerboseFormat lo = ansi setBlack <> "\n@(" <> displayCallStack cs <> ")"
     | otherwise = mempty

displayCallStack :: CallStack -> DisplayBuilder
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
    :: MVar ByteString -> LogOptions -> LogFunc
    -> (CallStack -> LogSource -> LogLevel -> LogStr -> IO ())
stickyImpl ref lo logFunc loc src level msgOrig = modifyMVar_ ref $ \sticky -> do
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

-- | With a sticky state, do the thing.
withSticky :: LogOptions -> (MVar ByteString -> IO b) -> IO b
withSticky lo inner = bracket
  (newMVar mempty)
  (\state -> do
      state' <- takeMVar state
      unless (B.null state') (logSend lo "\n"))
  inner
