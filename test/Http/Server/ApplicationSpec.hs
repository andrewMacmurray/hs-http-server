{-# LANGUAGE OverloadedStrings #-}

module Http.Server.ApplicationSpec where

import qualified Http.Server.Application       as A
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request (..))
import qualified Http.Server.Internal.Response as Res
import qualified Http.Server.Router            as R
import           Network.HTTP.Types            (StdMethod (..))
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
spec =
  describe "runApp" $ do
    context "with Routes" $ do
      it "dispatches a request against a collection of routes" $ do
        let req = Request GET "/foo" [] [] ""
        res <- A.runApp routes req
        Res.body res `shouldBe` "foo"
