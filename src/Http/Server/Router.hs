{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http.Server.Router
  ( get
  , put
  , post
  , head
  , options
  , routes
  , matchRoute
  , Uri
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
import           Network.HTTP.Types            (StdMethod (..))
import qualified Network.HTTP.Types            as N
import qualified Network.HTTP.Types.Header     as H
import           Prelude                       hiding (head)

newtype Routes =
  Routes (M.Map RoutePattern (Handler ()))

newtype Route =
  Route (RoutePattern, Handler ())

data RoutePattern = RoutePattern
  { method :: N.StdMethod
  , uri    :: Uri
  } deriving (Eq, Show, Ord)

type Uri = B.ByteString

-- Configuration functions for individual routes
get :: Uri -> Handler () -> Route
get = mkRoute GET

put :: Uri -> Handler () -> Route
put = mkRoute PUT

post :: Uri -> Handler () -> Route
post = mkRoute POST

head :: Uri -> Route
head uri = mkRoute HEAD uri H.respondOk

options :: Uri -> Handler () -> Route
options = mkRoute OPTIONS

mkRoute :: StdMethod -> Uri -> Handler () -> Route
mkRoute method uri h = Route (RoutePattern method uri, h)

routes :: [Route] -> Routes
routes = Routes . M.fromList . map (\(Route r) -> r)

-- Matching a Request with a collection of routes
matchRoute :: Routes -> Request -> Maybe (Handler ())
matchRoute routes req
  | isNotAllowed req routes = notAllowed
  | isOptionsRequest req = Just setAllowHeader
  | otherwise = matchedRoute
  where
    notAllowed = Just $ H.respond H.methodNotAllowed >> setAllowHeader
    matchedRoute = lookupRoute pattern routes
    setAllowHeader = withAllowedMethods routes pattern
    pattern = fromRequest req

lookupRoute :: RoutePattern -> Routes -> Maybe (Handler ())
lookupRoute pattern (Routes routes) = M.lookup pattern routes

isNotAllowed :: Request -> Routes -> Bool
isNotAllowed req routes = noMatch && hasOptions req routes
  where
    matchedRoute = lookupRoute (fromRequest req) routes
    noMatch = isNothing matchedRoute

isOptionsRequest :: Request -> Bool
isOptionsRequest req = Req.method req == OPTIONS

toOptionsRoute :: RoutePattern -> RoutePattern
toOptionsRoute pattern = pattern {method = OPTIONS}

hasOptions :: Request -> Routes -> Bool
hasOptions req routes = isJust $ lookupRoute opts routes
  where
    opts = toOptionsRoute . fromRequest $ req

withAllowedMethods :: Routes -> RoutePattern -> Handler ()
withAllowedMethods routes pattern = H.addHeaders [allowed]
  where
    allowed = (H.hAllow, intercalate "," methods)
    methods = N.renderStdMethod <$> allowedMethods routes pattern

allowedMethods :: Routes -> RoutePattern -> [StdMethod]
allowedMethods (Routes routes) pattern =
  nub . fmap method . filter sameUri $ M.keys routes
  where
    sameUri p = uri p == uri pattern

fromRequest :: Request -> RoutePattern
fromRequest Req.Request {..} = RoutePattern method uri
