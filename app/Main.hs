module Main where

import Server

main :: IO ()
main = do
    setUpConfig
    wooferListen

-- main = putStrLn "TEST"