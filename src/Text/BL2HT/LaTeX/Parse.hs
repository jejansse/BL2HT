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
) where

import Text.ParserCombinators.Parsec

import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- IsString instance


-- | Parses LaTeX command, returns (name, star, list of options or arguments).
command :: GenParser Char st ([Char], [Char], [[Char]])
command = do
  char '\\'
  name <- many1 letter
  star <- option "" (string "*")  -- some commands have starred versions
  args <- commandArgs
  return (name, star, args)

-- | Returns text between brackets and its matching pair.
bracketedText :: Char -> Char -> GenParser Char st [Char]
bracketedText openB closeB = do
  result <- charsInBalanced' openB closeB
  return $ [openB] ++ result ++ [closeB]

-- | Returns an option or argument of a LaTeX command.
optOrArg :: GenParser Char st [Char]
optOrArg = try $ spaces >> (bracketedText '{' '}' <|> bracketedText '[' ']')

-- | True if the string begins with '{'.
isArg :: [Char] -> Bool
isArg ('{':_) = True
isArg _       = False

-- | Returns list of options and arguments of a LaTeX command.
commandArgs :: GenParser Char st [[Char]]
commandArgs = many optOrArg

-- | Like @charsInBalanced@, but allow blank lines in the content.
charsInBalanced' :: Char -> Char -> GenParser Char st String
charsInBalanced' open close = try $ do
  char open
  raw <- many $       (many1 (satisfy $ \c -> c /= open && c /= close))
                  <|> (do res <- charsInBalanced' open close
                          return $ [open] ++ res ++ [close])
  char close
  return $ concat raw
