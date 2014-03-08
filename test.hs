#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified LispParser
import qualified DB

import Control.Monad (void)

import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do

    edb <- DB.dbParseFile "data.txt"
    case edb of
        Right db -> void $ runTestTT $ TestList [ LispParser.tests, (DB.tests db) ]
        Left e -> putStrLn $ show e
    quickCheck LispParser.propValue


