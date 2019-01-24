{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http.Server.Router
  ( get
  , put
  , post
  , head
  , options
  , router
  , routerM
  , runRouter
  , Uri
  , Route
  , Router
  ) where

import qualified Data.ByteString               as B
import           Data.ByteString.Char8         (intercalate)
import           Data.List                     (nub)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, isJust, isNothing)
import           Http.Server.Handler           (Handler, HandlerM)
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request)
import qualified Http.Server.Internal.Request  as Req
import           Http.Server.Internal.Response (Response)
import qualified Http.Server.Internal.Response as Res
import           Network.HTTP.Types            (StdMethod (..))
import qualified Network.HTTP.Types            as N
import qualified Network.HTTP.Types.Header     as H
import           Prelude                       hiding (head)

data Router a = Router
  { routes   :: M.Map RoutePattern (HandlerM a)
  , notFound :: HandlerM a
  }

newtype Route a =
  Route (RoutePattern, HandlerM a)

data RoutePattern = RoutePattern
  { method :: N.StdMethod
  , uri    :: Uri
  } deriving (Eq, Show, Ord)

type Uri = B.ByteString

-- Configuration functions for individual routes
get :: Uri -> HandlerM a -> Route a
get = mkRoute GET

put :: Uri -> HandlerM a -> Route a
put = mkRoute PUT

post :: Uri -> HandlerM a -> Route a
post = mkRoute POST

head :: Uri -> HandlerM a -> Route a
head = mkRoute HEAD

options :: Uri -> HandlerM a -> Route a
options = mkRoute OPTIONS

mkRoute :: StdMethod -> Uri -> HandlerM a -> Route a
mkRoute method uri h = Route (RoutePattern method uri, h)

router :: [Route ()] -> Router ()
router = routerM $ H.respond H.notFound

routerM :: HandlerM a -> [Route a] -> Router a
routerM notFoundHandler routes =
  Router
  { routes = M.fromList $ map (\(Route r) -> r) routes
  , notFound = notFoundHandler
  }

-- Match a Request against a Router
runRouter :: Router a -> Request -> HandlerM a
runRouter router@Router {..} = fromMaybe notFound . matchRoute router

matchRoute :: Router a -> Request -> Maybe (HandlerM a)
matchRoute router@Router {..} req
  | isNotAllowed router req = Just $ respondNotAllowed router req
  | isOptionsRequest req = withAllowHeaders
  | otherwise = matchedRoute
  where
    withAllowHeaders = (`H.around` setAllowHeader) <$> matchedRoute
    matchedRoute = lookupRoute (asPattern req) router
    setAllowHeader = withAllowedMethods router $ asPattern req

respondNotAllowed :: Router a -> Request -> HandlerM a
respondNotAllowed router@Router {..} request =
  H.around notFound withHeaders
  where
    withHeaders = H.respond H.methodNotAllowed >> setAllowHeader
    setAllowHeader = withAllowedMethods router $ asPattern request

lookupRoute :: RoutePattern -> Router a -> Maybe (HandlerM a)
lookupRoute routePattern Router {..} = M.lookup routePattern routes

isNotAllowed :: Router a -> Request -> Bool
isNotAllowed router req = noMatch && hasOptions router req
  where
    matchedRoute = lookupRoute (asPattern req) router
    noMatch = isNothing matchedRoute

isOptionsRequest :: Request -> Bool
isOptionsRequest req = Req.method req == OPTIONS

hasOptions :: Router a -> Request -> Bool
hasOptions router req = isJust $ lookupRoute opts router
  where
    opts = asOptions . asPattern $ req

asOptions :: RoutePattern -> RoutePattern
asOptions routePattern = routePattern {method = OPTIONS}

withAllowedMethods :: Router a -> RoutePattern -> Handler
withAllowedMethods router routePattern = H.addHeaders [allowed]
  where
    allowed = (H.hAllow, intercalate "," methods)
    methods = N.renderStdMethod <$> allowedMethods router routePattern

allowedMethods :: Router a -> RoutePattern -> [StdMethod]
allowedMethods Router {..} routePattern =
  nub . fmap method . filter (sameUri routePattern) $ M.keys routes
  where
    sameUri p1 p2 = uri p1 == uri p2

asPattern :: Request -> RoutePattern
asPattern Req.Request {..} = RoutePattern method uri
