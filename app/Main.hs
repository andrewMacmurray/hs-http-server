{-# LANGUAGE OverloadedStrings #-}

module Main where

import Http.Server         (serve)
import Http.Server.Handler (respond)

main :: IO ()
main = do
  putStrLn "listening on port 5000"
  serve 5000 app
  where
    app = respond "hello"
