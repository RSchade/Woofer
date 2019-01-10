module FSRead
    (
        getTextResource,
        getBinaryResource,
        validatePath,
        goesUp,
        getResource
    ) where

import Parser
import qualified Data.ByteString.Char8 as B
import System.FilePath
import System.Directory
import Config

gopherMapName = "gophermap"

-- Gets a text resource (gophermap compatibile) from a given file path
-- converts it into an array of Gopher statements
getTextResource :: Config -> FilePath -> IO [GopherStatement]
getTextResource cfg fname = do
    txt <- readFile fname
    return $ parseGMap cfg txt

-- Gets a binary (unparsed) resource from a given file path
getBinaryResource :: FilePath -> IO B.ByteString
getBinaryResource fname = B.readFile fname

-- Returns true if the given path has any '../' in it
goesUp :: FilePath -> Bool
goesUp p = or $ map (== "..") $ splitDirectories p

-- Validates the given file path
-- Paths must be direct (no ../)
-- They must point to a valid file
-- Returns a normalised version of the path given
validatePath :: FilePath -> FilePath -> FilePath
validatePath gopherDir rawPath
    | not $ isValid path  = error "Invalid path"
    | head rawPath /= '/' = error "Non-absolute path"
    | goesUp path         = error "Path backtracks"
    | otherwise           = fullPath
    where
        path = normalise rawPath
        fullPath = joinPath $ [gopherDir] ++ (tail $ splitDirectories rawPath)

-- Gets a resource and converts it to a Gopher response, 
-- chooses the correct function to use based on the resource path
-- If the given path points to a file, return that
-- If the given path points to a directory, generate a Gopher response based on that
getResource :: Config -> FilePath -> IO B.ByteString
getResource cfg rawPath = do
    let bind = netBind $ networkConf cfg
        port = netPort $ networkConf cfg
        gDir = gopherDir $ gopherConf cfg
        path = validatePath gDir rawPath
    if hasTrailingPathSeparator rawPath
    then
      do
          gMap <- getGopherMap cfg path rawPath
          return $ B.pack $ prepareResponse $ gMap
    else
      do
          binRes <- getBinaryResource path
          return binRes

-- Gets a gophermap for a particular directory, either by
-- finding it on disk or generating it
-- pass in a directory to generate/retrieve from and the raw path from the Gopher client
getGopherMap :: Config -> FilePath -> FilePath -> IO [GopherStatement]
getGopherMap cfg path rawPath = do
    let bind = netBind $ networkConf cfg
        port = netPort $ networkConf cfg
        gopherFilePath = path </> gopherMapName
    putStrLn gopherFilePath
    files <- listDirectory path
    let hasGMap = elem gopherMapName files
    if hasGMap
    then
      do
          gMap <- getTextResource cfg gopherFilePath
          return gMap
    else
        return $ map (\x -> buildGopherStatement bind port x (rawPath </> x)) files

-- Builds a statement based on the given file and path
buildGopherStatement :: String -> Int -> String -> String -> GopherStatement
buildGopherStatement defHost defPort file path = 
    Statement '0' (takeFileName file) $ Just (StatementPred path defHost defPort)
