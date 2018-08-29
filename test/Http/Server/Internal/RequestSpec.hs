{-# LANGUAGE OverloadedStrings #-}

module Http.Server.Internal.RequestSpec where

import qualified Data.ByteString              as B
import qualified Http.Server.Internal.Request as R
import           Network.HTTP.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "parseRequest" $ do
    it "parses HTTP method correctly" $ do
      let req = R.parseRequest "GET / HTTP/1.1\r\n\r\n"
      R.method <$> req `shouldBe` Just methodGet
      R.uri <$> req `shouldBe` Just "/"
    it "parses standard methods" $ do
      let req = R.parseRequest "POST /hello HTTP/1.1\r\n\r\n"
      R.method <$> req `shouldBe` Just methodPost
    it "parses uri with query param" $ do
      let req = R.parseRequest "GET /hello?foo=bar HTTP/1.1\r\n\r\n"
      R.params <$> req `shouldBe` Just [("foo", Just "bar")]
    it "parses uri with multiple params" $ do
      let req =
            R.parseRequest
              "GET /hello?foo=bar&bar=baz HTTP/1.1\r\n\r\n"
      R.params <$>
        req `shouldBe` Just [("foo", Just "bar"), ("bar", Just "baz")]
    it "parses encoded params" $ do
      let req =
            R.parseRequest
              "GET /hello?foo=Operators%20%3C%2C HTTP/1.1\r\n\r\n"
      R.params <$> req `shouldBe` Just [("foo", Just "Operators <,")]
    it "parses headers correctly" $ do
      let headers =
            "Content-Type: application-json\r\nConnection: Keep-Alive\r\n"
          req =
            R.parseRequest $
            mconcat ["GET / HTTP/1.1\r\n", headers, "\r\n"]
      R.headers <$>
        req `shouldBe`
        Just
          [ ("Content-Type", "application-json")
          , ("Connection", "Keep-Alive")
          ]
    it "parses a request with a body" $ do
      let req = R.parseRequest "GET / HTTP/1.1\r\n\r\nhello world"
      R.body <$> req `shouldBe` Just "hello world"
    it "rejects an invalid HTTP request" $
      R.parseRequest "Wut?" `shouldBe` Nothing
