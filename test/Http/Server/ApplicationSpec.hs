{-# LANGUAGE OverloadedStrings #-}

module Http.Server.ApplicationSpec where

import qualified Http.Server.Application       as A
import qualified Http.Server.Handler           as H
import           Http.Server.Internal.Request  (Request (..))
import qualified Http.Server.Internal.Response as Res
import qualified Http.Server.Router            as R
import           Network.HTTP.Types            (StdMethod (..))
import qualified Network.HTTP.Types            as N
import           Test.Hspec

routes :: R.Routes
routes =
  R.routes
    [ R.get "/foo" $ H.respondBS "foo"
    , R.put "/foo" $ H.respondBS "put foo"
    , R.get "/bar" $ H.respondBS "bar"
    , R.options "/foo" H.respondOk
    ]

spec :: Spec
spec =
  describe "runApp" $ do
    context "with single Handler" $ do
      it "dispatches a request against a given handler" $ do
        let req = Request GET "/hanlder" [] [] ""
        res <- A.runApp H.respondOk req
        Res.status res `shouldBe` N.status200
    context "with Routes" $ do
      it "dispatches a request against a collection of routes" $ do
        let req = Request GET "/foo" [] [] ""
        res <- A.runApp routes req
        Res.body res `shouldBe` "foo"
