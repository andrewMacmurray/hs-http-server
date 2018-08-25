{-# LANGUAGE OverloadedStrings #-}

module Main where

import Http.Server         (serve)
import Http.Server.Handler (respond)

main :: IO ()
main = serve 5000 app
  where
    app = const $ respond "hello"
