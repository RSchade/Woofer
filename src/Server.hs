{-# LANGUAGE OverloadedStrings #-}

module Server
    ( wooferListen,
      setUpConfig
    ) where

import Network.Simple.TCP
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import LoadEnv
import System.Environment (lookupEnv)

import Parser
import FSRead

data Config = Config {
    gopherDir:: String,
    bind :: String,
    port :: Int
} deriving(Show)

setUpConfig :: IO Config
setUpConfig = do
    loadEnv
    port <- lookupEnv "WOOFER_PORT"
    bind <- lookupEnv "WOOFER_BIND"
    gopherDir <- lookupEnv "WOOFER_GOPHER_DIR"
    return $ Config (fromMaybe "./gopher" gopherDir) 
                    (fromMaybe "localhost" bind)
                    (read (fromMaybe "70" port) :: Int)

-- Gets rid of a trailing newline
-- In Gopher all newlines are "/r/n"
pathNoNewline :: FilePath -> FilePath
pathNoNewline p = takeWhile (/= '\r') p

-- If the path is empty, add a slash
pathSlashIfEmpty :: FilePath -> FilePath
pathSlashIfEmpty p = if null p then "/" else p

-- Starts the main event loop for the server
-- Listens for requests for data and responds
-- with the correct response
wooferListen :: IO ()
wooferListen =  serve (Host "192.168.1.66") "70" $ \(connectionSocket, remoteAddr) -> do
    rawPath <- recv connectionSocket 300
    let condPath = pathSlashIfEmpty $ pathNoNewline $ B.unpack $ fromMaybe (B.pack "") rawPath
    putStrLn "RESP WITH PATH"
    putStrLn $ show $ map fromEnum $ B.unpack $ fromMaybe (B.pack "") rawPath
    putStrLn condPath
    putStrLn $ show $ map fromEnum condPath
    resource <- getResource condPath
    putStrLn $ show resource
    send connectionSocket resource