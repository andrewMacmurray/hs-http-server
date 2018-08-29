{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http.Server.Router
  ( get
  , put
  , post
  , options
  , routes
  , matchRoute
  , Route
  , Routes
  ) where

import qualified Data.ByteString               as B
import           Data.ByteString.Char8         (intercalate)
import           Data.List                     (nub)
import qualified Data.Map                      as M
import           Data.Maybe                    (isJust, isNothing)
import           Http.Server.Handler           (Handler)
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request)
import qualified Http.Server.Internal.Request  as Req
import           Http.Server.Internal.Response (Response)
import qualified Http.Server.Internal.Response as Res
import qualified Network.HTTP.Types            as N
import qualified Network.HTTP.Types.Header     as H

newtype Routes =
  Routes (M.Map RoutePattern Handler)

newtype Route =
  Route (RoutePattern, Handler)

data RoutePattern = RoutePattern
  { method :: N.Method
  , uri    :: Uri
  } deriving (Eq, Show, Ord)

type Uri = B.ByteString

-- Configuration functions for individual routes
get :: Uri -> Handler -> Route
get = mkRoute N.methodGet

put :: Uri -> Handler -> Route
put = mkRoute N.methodPut

post :: Uri -> Handler -> Route
post = mkRoute N.methodPost

options :: Uri -> Handler -> Route
options = mkRoute N.methodOptions

mkRoute :: N.Method -> Uri -> Handler -> Route
mkRoute method uri h = Route (RoutePattern method uri, h)

routes :: [Route] -> Routes
routes = Routes . M.fromList . map (\(Route r) -> r)

-- Matching a Request with a collection of routes
matchRoute :: Routes -> Request -> Maybe Handler
matchRoute routes req
  | isNotAllowed req routes = notAllowed
  | isOptionsRequest req = withAllowed <$> matchedRoute
  | otherwise = matchedRoute
  where
    notAllowed = Just . withAllowed $ const H.methodNotAllowed
    matchedRoute = lookupRoute routePattern routes
    withAllowed = withAllowedMethods routes routePattern
    routePattern = fromRequest req

lookupRoute :: RoutePattern -> Routes -> Maybe Handler
lookupRoute routePattern (Routes routes) =
  M.lookup routePattern routes

isNotAllowed :: Request -> Routes -> Bool
isNotAllowed req routes = noMatch && hasOptions req routes
  where
    matchedRoute = lookupRoute (fromRequest req) routes
    noMatch = isNothing matchedRoute

isOptionsRequest :: Request -> Bool
isOptionsRequest req = Req.method req == N.methodOptions

toOptionsRoute :: RoutePattern -> RoutePattern
toOptionsRoute routePattern = routePattern {method = N.methodOptions}

hasOptions :: Request -> Routes -> Bool
hasOptions req routes = isJust $ lookupRoute opts routes
  where
    opts = toOptionsRoute . fromRequest $ req

withAllowedMethods :: Routes -> RoutePattern -> Handler -> Handler
withAllowedMethods routes routePattern handler =
  (fmap . fmap) withAllowed handler
  where
    withAllowed res = res {Res.headers = allowed : Res.headers res}
    allowed = (H.hAllow, intercalate "," methods)
    methods = getAllowedMethods routes routePattern

getAllowedMethods :: Routes -> RoutePattern -> [N.Method]
getAllowedMethods (Routes routes) pattern =
  nub . map method . filter sameUri $ M.keys routes
  where
    sameUri p = uri p == uri pattern

fromRequest :: Request -> RoutePattern
fromRequest Req.Request {..} = RoutePattern method uri
