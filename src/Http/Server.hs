{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import qualified Control.Exception             as E
import           Control.Monad                 (forever)
import qualified Data.ByteString               as B
import           Http.Server.Handler           (Handler)
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request, parseRequest)
import           Http.Server.Internal.Response (Response, encodeResponse)
import           Http.Server.Internal.Socket

serve :: Int -> Handler -> IO ()
serve port handler = runServer handler serverSocket
  where
    serverSocket = listenOn port

runServer :: Handler -> IO ServerSocket -> IO ()
runServer handler serverSocket =
  E.bracket serverSocket closeServer loop
  where
    loop = forever . runRequest handler

runRequest :: Handler -> ServerSocket -> IO ()
runRequest handler ServerSocket {accept} =
  E.bracket accept close execRequest
  where
    execRequest sock = do
      readRequest sock >>= writeResponse sock handler
      close sock

writeResponse :: Socket -> Handler -> Maybe Request -> IO ()
writeResponse Socket {send} handler req =
  encodeResponse <$> response >>= send
  where
    response = runHandler handler req

readRequest :: Socket -> IO (Maybe Request)
readRequest Socket {receive} = parseRequest <$> receive maxBytes
  where
    maxBytes = 2024

runHandler :: Handler -> Maybe Request -> IO Response
runHandler handler req = execHandler `E.catch` errorResponse
  where
    execHandler = maybe H.badRequest handler req
    errorResponse e = return H.serverError (e :: E.SomeException)
