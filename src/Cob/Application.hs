{-# LANGUAGE OverloadedStrings #-}

module Cob.Application where

import qualified Http.Server.Middleware.Static as M
import qualified Http.Server.Router            as R

cobRoutes :: R.Routes
cobRoutes = do
  let sf = M.serveFile "public"
  R.routes
    [ R.head "/"
    , R.get "/file1" $ sf
    , R.get "/file2" $ sf
    , R.get "/image.jpeg" $ sf
    , R.get "/image.png" $ sf
    , R.get "/image.gif" $ sf
    ]
