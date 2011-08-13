{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Text.BL2HT.LaTeX.Parse
   Copyright   : Bart Coppens <kde@bartcoppens.be>, Jeroen Janssen <jejansse@gmail.com>
   License     : GNU GPL, version 2 or above 

   Maintainer  : Jeroen Janssen <jejansse@gmail.com>
   Stability   : alpha
   Portability : portable
   
   Initially based on some code from Pandoc, Copyright (C) 2006-2010 John MacFarlane (GPLv2+)

Parsing (some) LaTeX source code
-}


module Text.BL2HT.LaTeX.Parse (
      BasicTex(..)
    , parseDoc
) where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Control.Applicative

-- | Parses LaTeX command, returns (name, star, list of options or arguments).
parseCommand :: GenParser Char st BasicTex
parseCommand = do
  char '\\'
  name <- many1 letter
  optArgs <- parseOptArgs
  reqArgs <- parseReqArgs
  return $ Command name optArgs reqArgs

parseText :: GenParser Char st BasicTex
parseText = OtherText <$> noCommand
  where noCommand = many1 $ noneOf "\\{}[]"

-- | Parses optional commands argument
parseOptArgs :: GenParser Char st [BasicTex]
parseOptArgs = try parseOptArg <|> return []
  where 
    parseOptArg = do
      char '['
      arguments <- parseDoc
      char ']'
      return arguments

parseReqArgs :: GenParser Char st [BasicTex]
parseReqArgs = try parseReqArg <|> return []
  where
    parseReqArg = do
      char '{'
      arguments <- parseDoc
      char '}'
      return arguments

parseDoc :: GenParser Char st [BasicTex]
parseDoc = many $ parseCommand <|> parseText

---- | Returns text between brackets and its matching pair.
--bracketedText :: Char -> Char -> GenParser Char st String
--bracketedText openB closeB = do
--  result <- charsInBalanced' openB closeB
--  return $ [openB] ++ result ++ [closeB]
--
---- | Returns an option or argument of a LaTeX command.
--optOrArg :: GenParser Char st BasicTex
--optOrArg = try $ spaces >> (OtherText <$> bracketedText '{' '}' <|> OtherText <$> bracketedText '[' ']')
--
---- | True if the string begins with '{'.
--isArg :: String -> Bool
--isArg ('{':_) = True
--isArg _       = False
--
---- | Returns list of options and arguments of a LaTeX command.
--commandArgs :: GenParser Char st [BasicTex]
--commandArgs = many optOrArg
--
---- | Like @charsInBalanced@, but allow blank lines in the content.
--charsInBalanced' :: Char -> Char -> GenParser Char st String
--charsInBalanced' open close = try $ do
--  char open
--  raw <- many $       (many1 (satisfy $ \c -> c /= open && c /= close))
--                  <|> (do res <- charsInBalanced' open close
--                          return $ [open] ++ res ++ [close])
--  char close
--  return $ concat raw
--
data BasicTex =
      Command   { cmdName :: String
                , optArgs :: [BasicTex]
                , reqArgs :: [BasicTex]
                }
    | OtherText { texText :: String }
    deriving (Show, Eq)

-- | Parse a TeX document
--latexDocument = many token
--    where
--        token = Command   <$> parseCommand
--            <|> OtherText <$> nocommand
--        nocommand = many1 $ noneOf "\\"

