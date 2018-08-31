{-# LANGUAGE OverloadedStrings #-}

module Cob.Application where

import           Http.Server.Handler
import qualified Http.Server.Router  as R

cobRoutes :: R.Routes
cobRoutes = R.routes [R.head "/"]
