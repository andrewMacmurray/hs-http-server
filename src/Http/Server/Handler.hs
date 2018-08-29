{-# LANGUAGE OverloadedStrings #-}

module Http.Server.Handler where

import qualified Data.ByteString               as B
import           Http.Server.Internal.Request  (Request)
import           Http.Server.Internal.Response (Response (..))
import qualified Network.HTTP.Types            as N

type Handler = Request -> IO Response

-- Handlers
respondOk :: Handler
respondOk = respond ""

respond :: B.ByteString -> Handler
respond = const . return . Response N.status200 []

-- Default Responses
badRequest :: IO Response
badRequest = return $ Response N.status400 [] ""

notFound :: IO Response
notFound = return $ Response N.status404 [] ""

serverError :: IO Response
serverError = return $ Response N.status500 [] ""

methodNotAllowed :: IO Response
methodNotAllowed = return $ Response N.methodNotAllowed405 [] ""
