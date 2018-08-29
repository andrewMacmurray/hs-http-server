module Http.Server.Application where

import Http.Server.Handler
import Http.Server.Internal.Request
import Http.Server.Internal.Response
import Http.Server.Router

class Application a where
  runApp :: a -> Request -> IO Response

instance Application Routes where
  runApp = execRoute

execRoute :: Routes -> Request -> IO Response
execRoute routes req =
  case matchRoute routes req of
    Just h  -> h req
    Nothing -> notFound
