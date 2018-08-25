{-# LANGUAGE OverloadedStrings #-}

module Http.Server.Request
  ( Request(..)
  , parseRequest
  ) where

import           Control.Monad              (void)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import           Data.Attoparsec.Combinator (atEnd)
import           Data.ByteString            as B
import qualified Data.CaseInsensitive       as CI
import           Data.Monoid                ((<>))
import           Data.Word                  (Word8)
import           Network.HTTP.Types         hiding (parseMethod)

data Request = Request
  { method  :: Method
  , uri     :: B.ByteString
  , params  :: Query
  , headers :: [Header]
  , body    :: B.ByteString
  }

parseRequest :: B.ByteString -> Either String Request
parseRequest rawRequest = eitherResult $ feed result ""
  where
    result = parse parseRequest' rawRequest

parseRequest' :: Parser Request
parseRequest' = do
  method <- parseMethod
  skipSpace
  (uri, query) <- parseUri
  skipSpace
  httpVersion
  clrf
  headers <- parseHeaders
  body <- parseBody
  return $
    Request
    { method = method
    , uri = uri
    , headers = headers
    , params = query
    , body = body
    }

parseBody :: Parser ByteString
parseBody = do
  noBody <- atEnd
  if noBody
    then return ""
    else clrf >> takeByteString

parseHeaders :: Parser [Header]
parseHeaders = option [] $ many' parseHeader

parseHeader :: Parser Header
parseHeader = do
  name <- A.takeWhile $ notInClass ":\r\n"
  skipString ": "
  value <- A.takeWhile $ notInClass "\r"
  clrf
  return (CI.mk name, value)

parseMethod :: Parser Method
parseMethod = choice standardMethods
  where
    standardMethods = (string . renderStdMethod) <$> enumFrom GET

parseUri :: Parser (B.ByteString, Query)
parseUri = do
  slash <- string "/"
  uri <- A.takeWhile notSpace
  return . fromFullUri $ slash <> uri
  where
    notSpace = (/= 32)

fromFullUri :: B.ByteString -> (B.ByteString, Query)
fromFullUri fullUri =
  case splitUri of
    (uri:query:_) -> (uri, parseQuery query)
    (uri:_)       -> (uri, [])
  where
    splitUri = B.split questionMark fullUri
    questionMark = 63

httpVersion :: Parser ()
httpVersion = skipString "HTTP/1.1"

clrf :: Parser ()
clrf = skipString "\r\n"

skipSpace :: Parser ()
skipSpace = skipString " "

skipString :: B.ByteString -> Parser ()
skipString = void . string
