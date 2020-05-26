{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (withAsync)
import           Data.ByteString          (ByteString)
import           Data.Either
import           Database.Postgres.Temp
import           Exsqlain
import           Test.Hspec

main = hspec spec

spec = describe "exsqlain" $ do
  it "can run a query" $ do
    r <- runSession (\conn -> runQuery conn "select x.a from generate_series(1,20) as x(a);")
    case r of
      Right x -> putText "query is ok"
      _       -> print r

    -- don't want to specify the encoding too precisely, we're only going
    -- to send it on to the website
    r `shouldSatisfy` isRight

  it "can open a port" $ runSession $ \connstr -> do
    changePort 7777
    staticExplainQuery  connstr "select x.a from generate_series(1,200000) as x(a);"
    1 `shouldBe` 1

runSession :: (ByteString -> IO a)
           -> IO a
runSession f = either (error . show)  pure =<< do
  withDbCache $ \cache -> do
    withConfig (cacheConfig cache) $ \db -> f (toConnectionString db)
