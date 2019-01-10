{-# LANGUAGE OverloadedStrings #-}

module Config
    ( importConfig, 
    Config(..),
    NetworkConfig(..),
    GopherConfig(..) ) where

import Data.Ini.Config
import qualified Data.Text as T
import Data.Either (fromRight)

data Config = Config {
    networkConf :: NetworkConfig,
    gopherConf :: GopherConfig
} deriving(Eq, Show)

data NetworkConfig = NetworkConfig {
    netBind :: String,
    netPort :: Int
} deriving(Eq, Show)

data GopherConfig = GopherConfig {
    gopherDir :: FilePath
} deriving(Eq, Show)

defaultConfig = Config (NetworkConfig "127.0.0.1" 8000) (GopherConfig "~/gopher")

-- Configuration parser
configParser :: IniParser Config
configParser = do
    netCf <- section "NETWORK" $ do
        bind <- fieldOf "bind" string
        port <- fieldOf "port" number
        return NetworkConfig {netBind = bind, netPort = port}
    gphCf <- section "GOPHER" $ do
        GopherConfig <$> T.unpack <$> field "homedir"
    return Config { networkConf = netCf, gopherConf = gphCf }

-- Parses the config file using the parser
importConfig :: IO Config
importConfig = do
    rawConfig <- readFile "/home/ray/Documents/Projects/Woofer/gopher/config.ini"
    let config = fromRight defaultConfig $ parseIniFile (T.pack rawConfig) configParser
    return config