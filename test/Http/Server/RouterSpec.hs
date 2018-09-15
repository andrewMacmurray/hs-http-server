{-# LANGUAGE OverloadedStrings #-}

module Http.Server.RouterSpec where

import           Data.Maybe                    (fromMaybe)
import qualified Http.Server.Application       as A
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request (..))
import qualified Http.Server.Internal.Response as Res
import qualified Http.Server.Router            as R
import           Network.HTTP.Types            (StdMethod (..))
import qualified Network.HTTP.Types            as N
import qualified Network.HTTP.Types.Header     as H
import           Test.Hspec

routes :: R.Routes
routes =
  R.routes
    [ R.get "/foo" $ H.respondBS "foo"
    , R.put "/foo" $ H.respondBS "put foo"
    , R.get "/bar" $ H.respondBS "bar"
    , R.options "/foo" H.respondOk
    ]

respondNotFound :: H.Handler
respondNotFound = H.respond H.notFound

spec :: Spec
spec = do
  let runRouter = A.runApp routes
  describe "matchRoute" $ do
    it "matches a request against a collection of routes" $ do
      let res = runRouter $ Request GET "/foo" [] [] ""
      Res.body <$> res >>= shouldBe "foo"
    it "returns nothing if no route found" $ do
      let res = runRouter $ Request PUT "/bar" [] [] ""
      Res.status <$> res >>= shouldBe N.status404
    context "when requesting a route with options" $ do
      let expectedHeaders = [(H.hAllow, "GET,PUT,OPTIONS")]
      it "returns not allowed handler if requested method not found" $ do
        let req = Request POST "/foo" [] [] ""
            res = runRouter req
        Res.status <$> res >>= shouldBe N.methodNotAllowed405
        Res.headers <$> res >>= shouldBe expectedHeaders
      it "includes allowed methods header in options request handler" $ do
        let req = Request OPTIONS "/foo" [] [] ""
            res = runRouter req
        Res.status <$> res >>= shouldBe N.status200
        Res.headers <$> res >>= shouldBe expectedHeaders
