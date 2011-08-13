{-# LANGUAGE OverloadedStrings #-}

module Text.BL2HT.LaTeX.Tests (
    tests
) where

import Test.Framework
import Test.QuickCheck
import qualified Test.HUnit as H
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Text.ParserCombinators.Parsec hiding (many, (<|>))

import Text.BL2HT.LaTeX.Parse

testParse string expected = case parse parseDoc  "" string of
    Right r   -> expected == r
    otherwise -> False

testSingleOptArgument = testProperty "Single argument" $ testParse  "\\documentclass[12pt]{sth} a paper" $
    [ Command { cmdName = "documentclass",
                optArgs = [[ OtherText "12pt" ]],
                reqArgs = [[ OtherText "sth" ]] },
      OtherText " a paper" ]

testSingleArgument = testProperty "Single argument" $ testParse "\\section{A Section name} a section text" $
    [ Command { cmdName = "section",
                optArgs = [],
                reqArgs = [[ OtherText "A Section name" ]] },
      OtherText " a section text" ]

testSingleCommand = testProperty "Single command" $ testParse  "\\item this is an item" $
    [ Command { cmdName = "item",
                optArgs = [],
                reqArgs = [] },
      OtherText " this is an item" ]

testSinglyNestedCommand = testProperty "Singly nested command" $ testParse "\\author{A \\and B}" $
    [ Command { cmdName = "author",
                optArgs = [],
                reqArgs = [[
                    OtherText "A ",
                    Command "and" [] [],
                    OtherText " B"
                ]] } ]

testMoreArguments = testProperty "More than one argument" $ testParse "\\newtheorem{definition}{Definition}\\section{Intro}" $
    [ Command { cmdName = "newtheorem",
                optArgs = [],
                reqArgs = [ OtherText "definition", OtherText "Definition" ] },
      Command { cmdName = "section",
                optArgs = [],
                reqArgs = [ OtherText "Intro" ] } ]

tests :: [Test]
tests = [
      testSingleOptArgument
    , testSingleArgument
    , testSingleCommand
    , testSinglyNestedCommand
    , testMoreArguments
    ]

