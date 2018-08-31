{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cob.Application
import Http.Server     (serve)

main :: IO ()
main = do
  putStrLn "listening on port 5000"
  serve 5000 cobRoutes
