{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Http.Server.Handler where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString               as B
import           Http.Server.Internal.Request  (Request)
import           Http.Server.Internal.Response (Response (..))
import qualified Network.HTTP.Types            as N

type Handler = HandlerM ()

newtype HandlerM a = HandlerM
  { runHandler :: ReaderT Request (StateT Response IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState Response
             , MonadReader Request
             , MonadIO
             )

setStatus :: N.Status -> Handler
setStatus status = modify $ \res -> res {status = status}

setBody :: B.ByteString -> Handler
setBody body = modify $ \res -> res {body = body}

setHeaders :: [N.Header] -> Handler
setHeaders headers = modify $ \res -> res {headers = headers}

addHeaders :: [N.Header] -> Handler
addHeaders newHeaders = modify withHeaders
  where
    withHeaders res = res {headers = newHeaders <> headers res}

around :: HandlerM a -> HandlerM b -> HandlerM a
around handlerA handlerB = do
  a <- handlerA
  handlerB
  return a

-- Handlers
respondOk :: Handler
respondOk = respond ok

respondBS :: B.ByteString -> Handler
respondBS = respond . withBody

respond :: Response -> Handler
respond = put

-- Default Responses
ok :: Response
ok = withBody ""

withBody :: B.ByteString -> Response
withBody = Response N.status200 []

badRequest :: Response
badRequest = Response N.status400 [] ""

notFound :: Response
notFound = Response N.status404 [] ""

serverError :: Response
serverError = Response N.status500 [] ""

methodNotAllowed :: Response
methodNotAllowed = Response N.methodNotAllowed405 [] ""
