{-# LANGUAGE OverloadedStrings #-}

module Http.Server.RouterSpec where

import           Data.Maybe                    (fromMaybe)
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request (..))
import qualified Http.Server.Internal.Response as Res
import qualified Http.Server.Router            as R
import qualified Network.HTTP.Types            as N
import qualified Network.HTTP.Types.Header     as H
import           Test.Hspec

routes :: R.Routes
routes =
  R.routes
    [ R.get "/foo" $ H.respond "foo"
    , R.put "/foo" $ H.respond "put foo"
    , R.get "/bar" $ H.respond "bar"
    , R.options "/foo" H.respondOk
    ]

respondNotFound :: H.Handler
respondNotFound = const H.notFound

spec :: Spec
spec = do
  let runMatch = fromMaybe respondNotFound . R.matchRoute routes
  describe "matchRoute" $ do
    it "matches a request against a collection of routes" $ do
      let req = Request N.methodGet "/foo" [] [] ""
          handler = runMatch req
      Res.body <$> handler req >>= shouldBe "foo"
    it "returns nothing if no route found" $ do
      let req = Request N.methodPut "/bar" [] [] ""
          handler = runMatch req
      Res.status <$> handler req >>= shouldBe N.status404
    context "when requesting a route with options" $ do
      let expectedHeaders = [(H.hAllow, "GET,OPTIONS,PUT")]
      it "returns not allowed handler if requested method not found" $ do
        let req = Request N.methodPost "/foo" [] [] ""
            handler = runMatch req
            res = handler req
        Res.status <$> res >>= shouldBe N.methodNotAllowed405
        Res.headers <$> res >>= shouldBe expectedHeaders
      it "includes allowed methods header in options request handler" $ do
        let req = Request N.methodOptions "/foo" [] [] ""
            handler = runMatch req
            res = handler req
        Res.status <$> res >>= shouldBe N.status200
        Res.headers <$> res >>= shouldBe expectedHeaders
