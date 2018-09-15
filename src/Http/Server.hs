{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import qualified Control.Exception             as E
import           Control.Monad                 (forever)
import qualified Data.ByteString               as B
import           Http.Server.Application       (Application, runApp)
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request, parseRequest)
import           Http.Server.Internal.Response (Response, encodeResponse)
import           Http.Server.Internal.Socket

serve :: Application m => Int -> m a -> IO ()
serve port app = runServer app serverSocket
  where
    serverSocket = listenOn port

runServer :: Application m => m a -> IO ServerSocket -> IO ()
runServer app serverSocket = E.bracket serverSocket closeServer loop
  where
    loop = forever . runRequest app

runRequest :: Application m => m a -> ServerSocket -> IO ()
runRequest app ServerSocket {accept} =
  E.bracket accept close execRequest
  where
    execRequest sock = do
      readRequest sock >>= writeResponse sock app
      close sock

writeResponse ::
     Application m => Socket -> m a -> Maybe Request -> IO ()
writeResponse Socket {send} app req =
  encodeResponse <$> appResponse >>= send
  where
    appResponse = execApp app req

readRequest :: Socket -> IO (Maybe Request)
readRequest Socket {receive} = parseRequest <$> receive maxBytes
  where
    maxBytes = 2024

execApp :: Application m => m a -> Maybe Request -> IO Response
execApp app req = appResponse `E.catch` (return . errorResponse)
  where
    appResponse = maybe (return H.badRequest) (runApp app) req
    errorResponse e = return H.serverError (e :: E.IOException)
