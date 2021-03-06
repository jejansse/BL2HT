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
    , MathMode(..)
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
  where noCommand = many1 $ noneOf "\\{}[]$"

getTextMathMode = do
    dollars <- many $ char '$'
    return $ case dollars of
        ['$'] -> "$"
        ['$', '$'] -> "$$"
        _ -> fail "Unknown amount of dollars"

mathModeFromText "$"  = MathInline
mathModeFromText "$$" = MathBig
mathModeFromText _    = error "No such math mode"

parseMath :: GenParser Char st BasicTex
parseMath = try $ do
    mm <- getTextMathMode
    math <- many1 $ noneOf "$"
    string mm >> (lookAhead $ noneOf "$")
    return $ Maths (mathModeFromText mm) math

-- | Parses optional commands argument
parseOptArgs :: GenParser Char st [LatexDoc]
parseOptArgs = option [] parseOptArgs1
  where
    parseOptArgs1 = between (char '[') (char ']') (parseDoc `sepBy` char ',')

parseReqArgs :: GenParser Char st [LatexDoc]
parseReqArgs = option [] parseOptArgs1
  where
    parseOptArgs1 = between (char '{') (char '}') (parseDoc `sepBy` char ',')

parseDoc :: GenParser Char st [BasicTex]
parseDoc = many $ parseCommand <|> parseText <|> parseMath

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

data MathMode = MathInline | MathBig
    deriving (Show, Eq)

data BasicTex =
      Command   { cmdName :: String
                , optArgs :: [LatexDoc]
                , reqArgs :: [LatexDoc]
                }
    | Maths     { mathMode :: MathMode
                , mathText :: String
                }
    | OtherText { texText :: String }
    deriving (Show, Eq)

type LatexDoc = [BasicTex]

-- | Parse a TeX document
--latexDocument = many token
--    where
--        token = Command   <$> parseCommand
--            <|> OtherText <$> nocommand
--        nocommand = many1 $ noneOf "\\"

