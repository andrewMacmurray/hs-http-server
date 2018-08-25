{-# LANGUAGE OverloadedStrings #-}

module Http.Server.Handler where

import Data.ByteString      as B
import Http.Server.Request
import Http.Server.Response
import Network.HTTP.Types

type Handler = Request -> IO Response

respondOk :: IO Response
respondOk = respond ""

badRequest :: IO Response
badRequest = return $ Response status400 [] ""

serverError :: IO Response
serverError = return $ Response status500 [] ""

respond :: B.ByteString -> IO Response
respond = return . Response status200 []
