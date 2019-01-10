module Parser
    ( parseText,
      parseGMap,
      validateStatement,
      prepareResponse,
      parseTextLineP,
      parseCommentLineP,
      parseResourceLineP, 
      parseGMapLineP,
      GopherStatement(..),
      StatementPred(..)
    ) where

import Data.Maybe
import Data.List
import Text.ParserCombinators.ReadP as RP
import Control.Applicative
import Config

-- Contains functions that parse plain text files and convert them into Gopher syntax
-- Contains functions that parse gophermaps and convert them into Gopher syntax

data StatementPred = StatementPred { selector :: String, 
                                    hostname :: String, 
                                    port :: Int } deriving(Eq)

data GopherStatement =  Statement { 
                         itemType :: Char,
                         display :: String,
                         pred :: Maybe StatementPred }
                       | GopherComment { comment :: String } deriving(Eq)

defPred = StatementPred "fake" "(NULL)" 0

-- Line seperator
lineSep = "\r\n"
-- Statement seperator
stmtSep = "\t"
-- Comment denotation
cmtDen = '#'
-- Text line type
txtType = 'i'
-- max width
width = 67

-- footer for every page
footer = [(Statement txtType (take width $ repeat '_') Nothing),
          (Statement txtType "Served by Woofer" Nothing)]

instance Show StatementPred where
    show (StatementPred sel hst prt) = sel ++ stmtSep ++
                                       hst ++ stmtSep ++
                                       show prt

instance Show GopherStatement where
    show (Statement it disp pred) = [it] ++ disp ++ stmtSep ++ 
                                    (show (fromMaybe defPred pred))
                                    ++ lineSep
    show (GopherComment cmt) = [cmtDen] ++ cmt -- Comments shouldn't output anything

validTypes = ['0' .. '9'] ++ ['+', 'g', 'I', 'T', 'i', 'h', 's']

-- Validates a Gopher statement for sanity
-- Throws an error if there are invalid fields for the kind of statement specified
validateStatement :: GopherStatement -> GopherStatement
validateStatement (Statement it hst pred)
    | isNothing $ elemIndex it validTypes = error "Not a valid selector"
    | otherwise = Statement it hst pred
validateStatement line = line -- no validation necessary, 
                              -- can't put invalid characters on a comment line

-- Converts a paragraph of text into an array of Gopher statements
parseText :: String -> [GopherStatement]
parseText para = map (\line -> Statement 'i' line Nothing) $ lines para

-- Converts an array of Gopher statements into a validated Gopher response
-- suitable for transmitting over a network connection to a Gopher client
prepareResponse :: [GopherStatement] -> String
prepareResponse stmts = (concat $ map (\stmt -> show $ validateStatement stmt) (stmts ++ footer)) ++ "." ++ lineSep

-- Converts a gophermap into an array of Gopher statements
parseGMap :: Config -> String -> [GopherStatement]
parseGMap cfg gmap =
    removeComments $ map (\x -> fst $ head $ reverse $ readP_to_S (parseGMapLineP cfg) x) (lines gmap)

-- Remove comment lines from a list of Gopher statements
removeComments :: [GopherStatement] -> [GopherStatement]
removeComments stmts = 
    let isCmt (GopherComment _) = False; isCmt _ = True
    in filter isCmt stmts

-- If this line is a comment, return the comment string
parseCommentLineP :: ReadP GopherStatement
parseCommentLineP = do
    char cmtDen
    cmt <- RP.many get
    eof
    return (GopherComment cmt)

-- Scans if this is a text line
-- A text line has no tabs and isn't a comment line
parseTextLineP :: ReadP GopherStatement
parseTextLineP = do
   first <- satisfy (/= cmtDen)
   rest <- RP.many (satisfy (/= (head stmtSep)))
   eof
   return (Statement txtType ([first] ++ rest) Nothing)

getSep :: ReadP String
getSep = manyTill (get) (satisfy (== (head stmtSep)))

-- Returns something if this line is a resource
-- Resource lines have tabs and start with a valid type
parseResourceLineP :: String -> String -> ReadP GopherStatement
parseResourceLineP h p = do
    gType <- satisfy (\x -> elem x validTypes)
    disp <- getSep
    sel <- getSep <|> (many1 get)
    (host, port) <- parseDefHostPortP h p <|> parseHostPortP
    eof
    return (Statement gType disp $ Just (StatementPred sel host (read port :: Int)))

-- It's possible for a resource line to not have the host or port
-- This returns the default values for that
parseDefHostPortP :: String -> String -> ReadP (String, String)
parseDefHostPortP h p = do
    return (h, p)

parseHostPortP :: ReadP (String, String)
parseHostPortP = do
    host <- getSep
    port <- many1 get
    return (host, port)

parseBlankLineP :: ReadP GopherStatement
parseBlankLineP = do
    eof
    return (Statement txtType "" Nothing)

-- Parses a given gophermap, turns it into a list of GopherStatements
-- A gophermap is a text file that contains comments, text and resources
-- A comment is denoted by a #, these are ignored
-- Text has no tabs and is not a comment
-- A resource has tabs to denote the selector, hostname and port 
parseGMapLineP :: Config -> ReadP GopherStatement
parseGMapLineP cfg = do
    let host = netBind $ networkConf cfg
        port = show $ netPort $ networkConf cfg
    (parseCommentLineP <|> (parseResourceLineP host port) <|> parseTextLineP <|> parseBlankLineP)