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
    [ Command { texCommand = ( "documentclass", [ OtherText { texText = "[12pt]" },
                                                  OtherText { texText = "{sth}" }  ]) },
      OtherText { texText = " a paper" } ]

testSingleArgument = testProperty "Single argument" $ testParse "\\section{A Section name} a section text" $
    [ Command { texCommand = ( "section", [ OtherText { texText = "{A Section name}" } ] ) },
      OtherText { texText = " a section text" } ]

testSingleCommand = testProperty "Single command" $ testParse  "\\item this is an item" $
    [ Command { texCommand = ( "item", [] ) },
      OtherText { texText = " this is an item" } ]

testSinglyNestedCommand = testProperty "Singly nested command" $ testParse "\\author{A \\and B}" $
    [ Command { texCommand = ( "author", [
            OtherText { texText = "A " },
            Command   { texCommand = ( "and", [] ) },
            OtherText { texText = " B" }
        ] ) } ]

tests :: [Test]
tests = [
      testSingleOptArgument
    , testSingleArgument
    , testSingleCommand
    , testSinglyNestedCommand
    ]

