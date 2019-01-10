{-# LANGUAGE OverloadedStrings #-}

module Server
    ( wooferListen ) where

import Network.Simple.TCP
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.IO

import Parser
import FSRead
import Config

-- Gets rid of a trailing newline
-- In Gopher all newlines are "/r/n"
pathNoNewline :: FilePath -> FilePath
pathNoNewline = filter (\x -> and [x /= '\r', x /= '\n'])

-- If the path is empty, add a slash
pathSlashIfEmpty :: FilePath -> FilePath
pathSlashIfEmpty p = if null p then "/" else p

-- Starts the main event loop for the server
-- Listens for requests for data and responds
-- with the correct response
wooferListen :: Config -> IO ()
wooferListen cfg =
    let bind = netBind $ networkConf cfg
        port = show $ netPort $ networkConf cfg
    in
    serve (Host bind) port $ \(connectionSocket, remoteAddr) -> do
        rawPath <- recv connectionSocket 300
        let condPath = pathSlashIfEmpty $ pathNoNewline $ B.unpack $ fromMaybe (B.pack "") rawPath
        -- putStrLn "RESP WITH PATH"
        -- putStrLn $ show $ map fromEnum $ B.unpack $ fromMaybe (B.pack "") rawPath
        putStrLn condPath
        -- putStrLn $ show $ map fromEnum condPath
        resource <- getResource cfg condPath
        -- putStrLn $ show resource
        send connectionSocket resource
    