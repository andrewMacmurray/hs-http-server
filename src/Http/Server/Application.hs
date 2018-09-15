{-# LANGUAGE FlexibleInstances #-}

module Http.Server.Application where

import Control.Monad.Reader          (runReaderT)
import Control.Monad.State           (execStateT)
import Http.Server.Handler
import Http.Server.Internal.Request
import Http.Server.Internal.Response
import Http.Server.Router

class Application a where
  runApp :: a -> Request -> IO Response

instance Application Routes where
  runApp = runRoute

execHandler :: Handler -> Request -> IO Response
execHandler handler request = runResponse
  where
    runResponse = execStateT (runRequest request) ok
    runRequest = runReaderT $ runHandler handler

instance Application Handler where
  runApp = execHandler

runRoute :: Routes -> Request -> IO Response
runRoute routes req =
  case matchRoute routes req of
    Just handler -> execHandler handler req
    Nothing      -> return notFound
