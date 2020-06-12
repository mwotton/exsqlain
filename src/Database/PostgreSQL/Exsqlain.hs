{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS_GHC -fno-warn-deprecations      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Exsqlain where

import Prelude hiding (log)

import           Control.Concurrent.Async             (Async, async, cancel)
import           Control.Concurrent.Chan              (Chan, newChan, readChan,
                                                       writeChan)
import           Control.Exception                    (bracket)
import           Control.Monad                        ((<=<))
import           Data.Aeson                           (ToJSON, encode)
import qualified Data.ByteString.Char8                as BS8
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Lazy                 (toStrict)
import           Data.ByteString.Builder              (byteString,
                                                       lazyByteString)
import           Data.Text                            (Text)
import           Data.Text.Encoding                   (decodeUtf8)                 
import           Data.Default                         (def)
import           Database.PostgreSQL.LibPQ            (connectdb, exec, finish,
                                                       getvalue)
import           GHC.Generics                         (Generic)                 
import           Network.HTTP.Types.Status            (status404)
import           Network.Wai                          (Application, pathInfo,
                                                       responseLBS)
import           Network.Wai.EventSource              (ServerEvent (..),
                                                       eventSourceAppChan)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (Destination (..),
                                                       destination,
                                                       mkRequestLogger)
import           Network.Wai.Middleware.Rewrite       (rewriteRoot)
import           Network.Wai.Middleware.Routed        (routedMiddleware)
import qualified Network.Wai.Middleware.Static        as Static
import           System.IO                            (stderr)
import           System.IO.Unsafe                     (unsafePerformIO)

import           Paths_exsqlain

-- | this is probably the one you want - it uses a static, persistent channel,
--   so if you're running with ghcid, reloading your specs file will not break
--   the connection.
staticExplainQuery :: ByteString -> ByteString -> IO (Either Text ())
staticExplainQuery connstr query = explainQuery staticChan connstr query

-- currently no connection pooling or even persistent connections.
-- probably won't matter.
runQuery :: ByteString -> ByteString -> IO (Either Text ByteString)
runQuery connstr stmt =
  bracket (connectdb connstr) finish $ \conn ->
    exec conn (mkExplain stmt) >>= \case
      Just r -> maybe (Left "bad getvalue") Right <$> getvalue r 0 0
      Nothing -> pure $ Left "oops" -- something better here
  where
    mkExplain stmt = "EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) " <> stmt

data QueryPlan =
  QueryPlan
  { query :: Text
  , plan  :: Text
  }  deriving (Generic,Show)
instance ToJSON QueryPlan

explainQuery :: Chan ServerEvent -> ByteString -> ByteString -> IO (Either Text ())
explainQuery chan connstr q = do
  log (show (connstr,q))
  runQuery connstr q >>= logTagVia "after runquery" show >>=
    either (pure.Left)
      (sendMsg chan . QueryPlan (decodeUtf8 q) . decodeUtf8)

-- may want to make this more elaborate
--  - request ids
--  - send close message on shutdown
sendMsg :: Chan ServerEvent -> QueryPlan -> IO (Either Text ())
sendMsg chan = logTagVia "sent msg" show
  <=< fmap Right . writeChan chan
  .   ServerEvent (Just "visualise") Nothing . (\x->[x]) . (<> "\n") . byteString
  <=< logTagVia "event"  BS8.unpack . toStrict . encode



{-# NOINLINE staticChan #-}
staticChan :: Chan ServerEvent
staticChan = unsafePerformIO newChan

changePort :: Warp.Port -> IO ()
changePort = writeChan staticPort . NewPort

newtype NewPort = NewPort Warp.Port

{-# NOINLINE staticPort #-}
staticPort :: Chan NewPort
staticPort = unsafePerformIO $ do
  controlChan <- newChan
  _ <- async $ do
    NewPort p <- readChan controlChan
    server <- async $ mkServer staticChan p
    go p server controlChan
  pure controlChan

 where go oldPort server controlChan = do
         NewPort newPort <- readChan controlChan
         if newPort == oldPort then do
           go oldPort server controlChan
         else do
           cancel server
           newServer <- async $ mkServer staticChan newPort
           go newPort newServer controlChan

mkServer :: Chan ServerEvent -> Warp.Port -> IO ()
mkServer chan port  = do
  staticFiles <- getDataFileName "js/dist"
  logger <- mkRequestLogger def{ destination=Handle stderr }
  Warp.run port
    . logger
    . rewriteRoot "index.html"
    . Static.staticPolicy (Static.addBase staticFiles)
    $ respondOn (== ["queryChan"]) chan

respondOn :: ([Text] -> Bool) -> Chan ServerEvent -> Application
respondOn pathPred chan req respond
 | pathPred (pathInfo req) = eventSourceAppChan chan req respond
 | otherwise = respond $ responseLBS status404 [] ""

log :: String -> IO ()
log = const (pure ())

logTagVia tag f x = log (tag <> ": " <> f x) >> pure x
