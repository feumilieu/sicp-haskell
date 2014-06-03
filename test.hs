#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified LispParser
import qualified DB

import Test.Tasty

main :: IO ()
main = do
    edb <- DB.dbParseFile "data.txt"
    case edb of
        Left e -> print e
        Right db -> defaultMain $ testGroup "Tests" [ LispParser.tests, LispParser.propValue, DB.tests db ]
