{-# LANGUAGE FlexibleInstances #-}

module Http.Server.Application where

import Control.Monad.Reader          (runReaderT)
import Control.Monad.State           (execStateT)
import Http.Server.Handler
import Http.Server.Internal.Request
import Http.Server.Internal.Response
import Http.Server.Router

class Application m where
  runApp :: m a -> Request -> IO Response

instance Application HandlerM where
  runApp = execHandler

instance Application Router where
  runApp routes request = execHandler handler request
    where
      handler = runRouter routes request

execHandler :: HandlerM a -> Request -> IO Response
execHandler handler request = runResponse
  where
    runResponse = execStateT (runRequest request) ok
    runRequest = runReaderT $ runHandler handler
