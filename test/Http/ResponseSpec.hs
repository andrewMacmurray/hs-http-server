{-# LANGUAGE OverloadedStrings #-}

module Http.ResponseSpec where

import qualified Http.Server.Response as R
import           Network.HTTP.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "encodeResponse" $ do
    it "renders a HTTP response" $ do
      let res =
            R.encodeResponse $
            R.Response
            {R.status = status200, R.headers = [], R.body = ""}
      res `shouldBe` "HTTP/1.1 200 OK\r\n\r\n"
    it "adds correct headers to response" $ do
      let res =
            R.encodeResponse $
            R.Response
            { R.status = status200
            , R.headers =
                [(hContentType, "text/html"), (hAccept, "text/html")]
            , R.body = ""
            }
      res `shouldBe`
        "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nAccept: text/html\r\n\r\n"
    it "adds correct body with content length" $ do
      let res =
            R.encodeResponse $
            R.Response
            { R.status = status200
            , R.headers = []
            , R.body = "hello world"
            }
      res `shouldBe`
        "HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello world"
