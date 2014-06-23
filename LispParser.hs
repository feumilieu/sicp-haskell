{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-} -- for parsec 3

-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
-- http://lstephen.wordpress.com/2007/07/29/parsec-parser-testing-with-quickcheck/

-- TODO: simple names (without lisp...)
-- TODO: use ReadPrec (?)
-- TODO: errors for parsers (<?>)

module LispParser
  ( Value (..)
  , lispExpr
  , lispSpace
  , parseFile
  , toDoc
  , propValue
  , tests
  ) where

import Data.Char ( isSpace, digitToInt )
import Control.Applicative hiding ( (<|>), many )
import Control.Monad (void, liftM)

import Text.Parsec
import Text.Parsec.Text ()
import qualified Data.Text.IO as T

import Text.PrettyPrint ( (<+>), (<>) )
import qualified Text.PrettyPrint as PP

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

data Value = Atom String
  | Bool Bool 
  | Integer Integer
  | String String
  | Nil
  | Pair Value Value
  deriving (Eq, Ord)

instance Show Value where
  show x = show $ toDoc x

instance Read Value where
  readsPrec _ = either (const []) id . parse parsecRead' ""
    where
      parsecRead' = fmap (:[]) $ (,) <$> (lispSpace >> lispExpr) <*> getInput

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

whitespace, lineComment, nestedComment, lispSpace :: Stream s m Char => ParsecT s u m ()
whitespace        = void $ satisfy isSpace
lineComment       = char ';' >> (void $ manyTill anyToken $ void (oneOf "\n\t") <|> eof)
nestedComment     = try (string "#|") >> inNestedComment
  where
    inNestedComment   = (void $ try $ string "|#")
      <|> (nestedComment >> inNestedComment)
      <|> (skipMany1 (noneOf "#|") >> inNestedComment)
      <|> (oneOf "#|" >> inNestedComment)
lispSpace             = skipMany $ whitespace <|> lineComment <|> nestedComment

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme t          = t <* lispSpace

idInitialChars, idSubsequentChars :: [Char]
idInitialChars    = "!$%&*/:<=>?^_~"
idSubsequentChars = "+-.@"

lispAtom :: Stream s m Char => ParsecT s u m Value
lispAtom          = Atom <$> lexeme ((:) <$> initial <*> many subsequent)
  where
    initial           = letter <|> oneOf idInitialChars
    subsequent        = initial <|> digit <|> oneOf idSubsequentChars

lispBool :: Stream s m Char => ParsecT s u m Value
lispBool          = Bool <$> lexeme (char '#' >> ((char 't' >> return True) <|> (char 'f' >> return False)))

lispInteger :: Stream s m Char => ParsecT s u m Value
lispInteger       = Integer <$> lexeme int
  where
    makeInt           = foldl (\x y -> x * 10 + y) 0
    sign              = (char '-' >> return negate)
      <|> (char '+' >> return id)
      <|> return id
    int               = sign <*> nat
    nat               = makeInt <$> many1 (fromIntegral . digitToInt <$> digit)

lispString :: Stream s m Char => ParsecT s u m Value
lispString        = String <$> lexeme (between (char '\"') (char '\"') stringInternal)
  where
    stringEscapes     = char '\"'
      <|> char '\\'
      <|> '\a' <$ char 'a'
      <|> '\b' <$ char 'b'
      <|> '\t' <$ char 't'
      <|> '\n' <$ char 'n'
      <|> '\r' <$ char 'r'
    stringInternal    = many $ noneOf "\\\"" <|> (char '\\' >> stringEscapes)

lispPair :: Stream s m Char => ParsecT s u m Value
lispPair          = lexeme (char '(') >> inlispPairL
  where
    inlispPairR l     = (lexeme (char ')') >> return (Pair l Nil))
      <|> (lexeme (char '.') >> (Pair l <$> lispExpr) <* lexeme (char ')'))
      <|> (Pair l <$> (lispExpr >>= inlispPairR) )
    inlispPairL       = lexeme (char ')' >> return Nil)
      <|> (lispExpr >>= inlispPairR)

lispExpr :: Stream s m Char => ParsecT s u m Value
lispExpr          = lispAtom <|> lispBool <|> lispInteger <|> lispString <|> lispPair

parseFile :: String -> IO (Either ParseError [Value])
parseFile fname =  runP (lispSpace >> many LispParser.lispExpr <* eof) () fname `liftM` T.readFile fname

-------------------------------------------------------------------------------
-- Pretty print
-------------------------------------------------------------------------------

toDoc :: Value -> PP.Doc
toDoc (Atom s) = PP.text s
toDoc (Bool True) = PP.text "#t"
toDoc (Bool False) = PP.text "#f"
toDoc (Integer i) = PP.text $ show i
toDoc (String s) = PP.doubleQuotes $ PP.text (escapeString s)
  where
    escapeFoldFunction '\"' = ("\\\"" ++)
    escapeFoldFunction '\\' = ("\\\\" ++)
    escapeFoldFunction c    = (c :)
    escapeString = foldr escapeFoldFunction []

toDoc Nil = PP.text "()"
toDoc (Pair l Nil) = PP.parens $ toDoc l
toDoc (Pair l r) = PP.char '(' <> toDoc l <+> toDocR r 
  where
    toDocR (Pair ll Nil) = toDoc ll <> PP.char ')'
    toDocR (Pair ll ls@(Pair _ _)) = toDoc ll <+> toDocR ls
    toDocR (Pair ll rr) = toDoc ll <+> PP.char '.' <+> toDoc rr <> PP.char ')'
    toDocR x = PP.char '.' <+> toDoc x <> PP.char ')'

-------------------------------------------------------------------------------
-- QuickCheck generator
-------------------------------------------------------------------------------

instance Arbitrary Value where
  arbitrary = genValue
    where
      genChar = elements $ ['a'..'z'] ++ ['A'..'Z']
      genInitChar = frequency [(5, genChar), (1, elements idInitialChars)]
      genSubsequentChar = frequency [(5, genChar), (1, elements idInitialChars), (1, elements idSubsequentChars)]
      genAtom = Atom <$> ((:) <$> genInitChar <*> listOf genSubsequentChar)

      genBool = Bool <$> arbitrary

      genInteger = Integer <$> arbitrary

      genString = String <$> listOf arbitrary
      -- genString = String <$> (listOf $ frequency [(20, genChar), (1, elements idInitialChars), (1, elements idSubsequentChars), (3, elements[' '])])

      genNil = elements [ Nil ]

      genValue = sized genValue'
      genValue' 0 = oneof [ genAtom, genBool, genInteger, genString, genNil ]
      genValue' n = Pair <$> (choose (0, n - 1) >>= genValue') <*> (choose (0, n - 1) >>= genValue')


-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LispParser internal"
  [ testCase "whitespace1"     $ parseOK    whitespace      " "             ()
  , testCase "whitespace2"     $ parseOK    whitespace      "\n"            ()
  , testCase "lineComment1"    $ parseOK    lineComment     "; привет \r"   ()
  , testCase "lineComment2"    $ parseOK    lineComment     "; привет "     ()
  , testCase "nestedComment1"  $ parseOK    nestedComment   "#||#"          ()
  , testCase "nestedComment2"  $ parseOK    nestedComment   "#|#||#|#"      ()
  , testCase "nestedComment2"  $ parseFail  nestedComment   "#|#||#|# "
  , testCase "nestedComment3"  $ parseFail  nestedComment   "#|"
  , testCase "nestedComment4"  $ parseFail  nestedComment   "#||"
  , testCase "nestedComment5"  $ parseFail  nestedComment   "#|#|"
  , testCase "nestedComment6"  $ parseFail  nestedComment   "#|#||#|"
  , testCase "space"           $ parseOK    lispSpace     "   \n\t #|  Привет, как дела? #|!!!|# |# ; \r ;  Ура!"    ()
  , testCase "ident"           $ parseOK    lispAtom      "!013-x ; Вот!" (Atom "!013-x")
  , testCase "ident"           $ parseFail  lispAtom      ".xx"
  , testCase "bool1"           $ parseOK    lispBool      "#t "           (Bool True)
  , testCase "bool2"           $ parseOK    lispBool      "#f "           (Bool False)
  , testCase "Integer1"        $ parseOK    lispInteger   "123 "          (Integer 123)
  , testCase "Integer2"        $ parseOK    lispInteger   "-0666 ; FIXME" (Integer (-666))
  , testCase "string1"         $ parseOK    lispString    "\"Wow!\" #| Here you are |# " (String "Wow!")
  , testCase "string2"         $ parseOK    lispString    "\"\\\"\\\\\\t\" "  (String "\"\\\t")
  , testCase "string3"         $ parseFail  lispString    "\\w "
  , testCase "pair1"           $ parseOK    lispPair      "( ) "          Nil
  , testCase "pair1x"          $ parseOK    lispPair      "(()) "         (Pair Nil Nil)
  , testCase "pair2"           $ parseOK    lispPair      "( x ) "        (Pair (Atom "x") Nil)
  , testCase "pair3"           $ parseOK    lispPair      "( -23) "       (Pair (Integer (-23)) Nil)
  , testCase "pair4"           $ parseOK    lispPair      "(\"oops\") "   (Pair (String "oops") Nil)
  , testCase "pair5"           $ parseOK    lispPair      "(() \"oops\")" (Pair Nil (Pair (String "oops") Nil))
  , testCase "pair6"           $ parseOK    lispPair      "(().()) "      (Pair Nil Nil)
  , testCase "pair7"           $ parseOK    lispPair      "(().\"oops\")" (Pair Nil (String "oops"))
  , testCase "pair8"           $ parseOK    lispPair      "(\"oops\".())" (Pair (String "oops") Nil)
  , testCase "pair9"           $ parseOK    lispPair      "( #t . () ) "  (Pair (Bool True) Nil)
  , testCase "pair10"          $ parseOK    lispPair      "( () . #f ) "  (Pair Nil (Bool False))
  , testCase "pair11"          $ parseOK    lispPair      "(a b c) "      (Pair (Atom "a") (Pair (Atom "b") (Pair (Atom "c") Nil)))
  , testCase "pair12"          $ parseOK    lispPair      "(a b . c) "    (Pair (Atom "a") (Pair (Atom "b") (Atom "c")))
  , testCase "pair13"          $ parseFail  lispPair      "(a . b c) "
  ]
  where
    parseOK parser str expected = case parse (parser <* eof) "" str of
      Left e -> assertFailure $ show e
      Right actual -> assertEqual "" expected actual

    parseFail parser str = case parse (parser <* eof) "" str of
      Left _ -> return ()
      Right x -> assertFailure $ "parser returned: " ++ show x

propValue' :: Value -> Bool
propValue' x = either (const False) (== x) $ parse lispExpr "" $ show $ toDoc x

propValue :: TestTree
propValue = QC.testProperty "LispParser QuickCheck" propValue'
