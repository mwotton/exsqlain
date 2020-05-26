{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS_GHC -fno-warn-deprecations      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Exsqlain where

import           Control.Concurrent.Async             (Async, async, cancel)
import           Control.Concurrent.Chan              (Chan, newChan, readChan,
                                                       writeChan)
import           Control.Exception                    (bracket)
import           Data.Aeson                           (ToJSON, encode)
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Builder              (byteString,
                                                       lazyByteString)
import           Data.Default                         (def)
import           Database.PostgreSQL.LibPQ            (connectdb, exec, finish,
                                                       getvalue)
import           Exsqlain.Static
import           Network.HTTP.Types.Status            (status404)

import           Network.Wai                          (Application, pathInfo,
                                                       responseLBS)
import           Network.Wai.EventSource              (ServerEvent (..),
                                                       eventSourceAppChan)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (Destination (..),
                                                       destination,
                                                       mkRequestLogger)
import           Network.Wai.Middleware.Routed        (routedMiddleware)
import qualified Network.Wai.Middleware.Static        as Static
import           System.IO.Unsafe                     (unsafePerformIO)

import           Paths_exsqlain

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

staticExplainQuery = explainQuery staticChan

explainQuery :: Chan ServerEvent -> ByteString -> ByteString -> IO (Either Text ())
explainQuery chan connstr q = do
  log (show (connstr,q))
  runQuery connstr q >>= logTagVia "after runquery" show >>=
    either (pure.Left)
      (sendMsg chan . QueryPlan (decodeUtf8 q) . decodeUtf8)


-- staticSendMsg :: QueryPlan -> IO (Either Text ())
-- staticSendMsg = sendMsg staticChan

sendMsg :: Chan ServerEvent -> QueryPlan -> IO (Either Text ())
sendMsg chan = logTagVia "sent msg" show
  <=< fmap Right . writeChan chan
  .   ServerEvent (Just "visualise") Nothing . (\x->[x]) . (<> "\n") . byteString
  <=< logTagVia "event"  decodeUtf8 . fromLazy . encode

    -- may want to make this more elaborate
    --  - request ids
    --  - send close message on shutdown


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
    . Static.staticPolicy (Static.addBase staticFiles)
    $ respondOn (== ["queryChan"]) chan
--    $ eventSourceAppChan chan


respondOn :: ([Text] -> Bool) -> Chan ServerEvent -> Application
respondOn pathPred chan req respond
 | pathPred (pathInfo req) = eventSourceAppChan chan req respond
 | otherwise = respond $ responseLBS status404 [] ""


-- log = appendFile "/home/mark/sqllogs" . (<>"\n\n")

log :: String -> IO ()
log = const (pure ())

logTagVia tag f x = log (tag <> ": " <> f x) >> pure x
