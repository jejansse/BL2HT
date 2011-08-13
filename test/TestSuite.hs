module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Text.BL2HT.LaTeX.Tests

main :: IO ()
main = defaultMain
    [
        testGroup "Text.BL2HT.LaTeX.Tests" Text.BL2HT.LaTeX.Tests.tests
    ]
