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

type Handler = MonadHandler ()

newtype MonadHandler a = MonadHandler
  { runHandler :: ReaderT Request (StateT Response IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState Response
             , MonadReader Request
             , MonadIO
             )

setStatus :: N.Status -> Handler
setStatus s = modify $ \res -> res {status = s}

setBody :: B.ByteString -> Handler
setBody b = modify $ \res -> res {body = b}

setHeaders :: [N.Header] -> Handler
setHeaders hs = modify $ \res -> res {headers = hs}

addHeaders :: [N.Header] -> Handler
addHeaders hs = modify $ \res -> res {headers = hs ++ headers res}

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
