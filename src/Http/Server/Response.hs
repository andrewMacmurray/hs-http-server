{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http.Server.Response
  ( Response(..)
  , encodeResponse
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Data.CaseInsensitive  as CI
import           Network.HTTP.Types

data Response = Response
  { status  :: Status
  , headers :: [Header]
  , body    :: B.ByteString
  }

encodeResponse :: Response -> B.ByteString
encodeResponse Response {..} =
  mconcat
    [ "HTTP/1.1 "
    , renderStatus status
    , renderHeaders combinedHeaders
    , "\r\n"
    , body
    ]
  where
    combinedHeaders = headers ++ contentLengthHeader body

renderHeaders :: [Header] -> B.ByteString
renderHeaders = foldr (B.append . renderHeader) ""

renderHeader :: Header -> B.ByteString
renderHeader (name, value) =
  mconcat [CI.original name, ": ", value, "\r\n"]

contentLengthHeader :: B.ByteString -> [Header]
contentLengthHeader body =
  case B.length body of
    0 -> []
    n -> [(hContentLength, C.pack $ show n)]

renderStatus :: Status -> B.ByteString
renderStatus Status {..} =
  mconcat [render statusCode, " ", statusMessage, "\r\n"]
  where
    render = C.pack . show
