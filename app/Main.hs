module Main where

import Server
import Config

main :: IO ()
main = do
    wooferListen =<< importConfig