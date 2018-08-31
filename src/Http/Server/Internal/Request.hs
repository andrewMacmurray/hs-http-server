{-# LANGUAGE OverloadedStrings #-}

module Http.Server.Internal.Request
  ( Request(..)
  , parseRequest
  ) where

import           Control.Monad              (void)
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Combinator as AC
import           Data.ByteString            as B
import qualified Data.CaseInsensitive       as CI
import           Network.HTTP.Types         hiding (parseMethod)

data Request = Request
  { method  :: StdMethod
  , uri     :: B.ByteString
  , params  :: Query
  , headers :: [Header]
  , body    :: B.ByteString
  } deriving (Eq, Show)

parseRequest :: B.ByteString -> Maybe Request
parseRequest rawRequest = A.maybeResult $ A.feed result ""
  where
    result = A.parse parseRequest' rawRequest

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
  return
    Request
    { method = method
    , uri = uri
    , headers = headers
    , params = query
    , body = body
    }

parseBody :: Parser ByteString
parseBody = do
  noBody <- AC.atEnd
  if noBody
    then return ""
    else clrf >> A.takeByteString

parseHeaders :: Parser [Header]
parseHeaders = A.option [] $ A.many' parseHeader

parseHeader :: Parser Header
parseHeader = do
  name <- A.takeWhile $ A.notInClass ":\r\n"
  skipString ": "
  value <- A.takeWhile $ A.notInClass "\r"
  clrf
  return (CI.mk name, value)

parseMethod :: Parser StdMethod
parseMethod = A.choice standardMethods
  where
    standardMethods = parseStandard <$> enumFrom GET
    parseStandard m = A.string (renderStdMethod m) >> return m

parseUri :: Parser (B.ByteString, Query)
parseUri = do
  slash <- A.string "/"
  uri <- A.takeWhile notSpace
  return . fromFullUri $ mconcat [slash, uri]
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
skipString = void . A.string
