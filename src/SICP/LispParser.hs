{-# LANGUAGE FlexibleContexts #-} -- for parsec 3

-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
-- http://lstephen.wordpress.com/2007/07/29/parsec-parser-testing-with-quickcheck/

-- TODO: use ReadPrec (?)
-- TODO: errors for parsers (<?>)

module SICP.LispParser
  ( Value (..)
  , space, atom, bool, integer, string, pair, expr
  , parseFile
  , toDoc
  , tests
  ) where

import Data.Char ( isSpace, digitToInt )
import Control.Monad (void, liftM)

import           Text.Parsec hiding (space, string)
import qualified Text.Parsec as P (string)

import qualified Data.Text.IO as T

import           Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as PP

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

import Prelude hiding (fail)

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
      parsecRead' = fmap (:[]) $ (,) <$> (space >> expr) <*> getInput

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

whitespace, lineComment, nestedComment, space :: Stream s m Char => ParsecT s u m ()
whitespace        = void $ satisfy isSpace
lineComment       = char ';' >> (void $ manyTill anyToken $ void newline <|> eof)
nestedComment     = try (P.string "#|") >> inNestedComment
  where
    inNestedComment   = (void $ try $ P.string "|#")
      <|> (nestedComment >> inNestedComment)
      <|> (skipMany1 (noneOf "#|") >> inNestedComment)
      <|> (oneOf "#|" >> inNestedComment)
space             = skipMany $ whitespace <|> lineComment <|> nestedComment

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme t          = t <* space

idInitialChars, idSubsequentChars :: [Char]
idInitialChars    = "!$%&*/:<=>?^_~"
idSubsequentChars = "+-.@"

atom :: Stream s m Char => ParsecT s u m Value
atom          = Atom <$> lexeme ((:) <$> initial <*> many subsequent)
  where
    initial           = letter <|> oneOf idInitialChars
    subsequent        = initial <|> digit <|> oneOf idSubsequentChars

bool :: Stream s m Char => ParsecT s u m Value
bool                  = Bool <$> lexeme (char '#' >> ((char 't' >> return True) <|> (char 'f' >> return False)))

integer :: Stream s m Char => ParsecT s u m Value
integer               = Integer <$> lexeme int
  where
    makeInt           = foldl (\x y -> x * 10 + y) 0
    sign              = (char '-' >> return negate)
      <|> (char '+' >> return id)
      <|> return id
    int               = sign <*> nat
    nat               = makeInt <$> many1 (fromIntegral . digitToInt <$> digit)

string :: Stream s m Char => ParsecT s u m Value
string        = String <$> lexeme (between (char '\"') (char '\"') stringInternal)
  where
    stringEscapes     = char '\"'
      <|> char '\\'
      <|> '\a' <$ char 'a'
      <|> '\b' <$ char 'b'
      <|> '\t' <$ char 't'
      <|> '\n' <$ char 'n'
      <|> '\r' <$ char 'r'
    stringInternal    = many $ noneOf "\\\"" <|> (char '\\' >> stringEscapes)

pair :: Stream s m Char => ParsecT s u m Value
pair          = lexeme (char '(') >> inlispPairL
  where
    inlispPairR l     = (lexeme (char ')') >> return (Pair l Nil))
      <|> (lexeme (char '.') >> (Pair l <$> expr) <* lexeme (char ')'))
      <|> (Pair l <$> (expr >>= inlispPairR) )
    inlispPairL       = lexeme (char ')' >> return Nil)
      <|> (expr >>= inlispPairR)

expr :: Stream s m Char => ParsecT s u m Value
expr          = atom <|> bool <|> integer <|> string <|> pair

parseFile :: String -> IO (Either ParseError [Value])
parseFile fname =  runP (space >> many expr <* eof) () fname `liftM` T.readFile fname

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
  [ testCase "whitespace1"     $ ok    whitespace      " "             ()
  , testCase "whitespace2"     $ ok    whitespace      "\n"            ()
  , testCase "lineComment1"    $ ok    lineComment     "; привет \r"   ()
  , testCase "lineComment2"    $ ok    lineComment     "; привет "     ()
  , testCase "nestedComment1"  $ ok    nestedComment   "#||#"          ()
  , testCase "nestedComment2"  $ ok    nestedComment   "#|#||#|#"      ()
  , testCase "nestedComment2"  $ fail  nestedComment   "#|#||#|# "
  , testCase "nestedComment3"  $ fail  nestedComment   "#|"
  , testCase "nestedComment4"  $ fail  nestedComment   "#||"
  , testCase "nestedComment5"  $ fail  nestedComment   "#|#|"
  , testCase "nestedComment6"  $ fail  nestedComment   "#|#||#|"
  , testCase "space"           $ ok    space     "   \n\t #|  Привет, как дела? #|!!!|# |# ; \r ;  Ура!"    ()
  , testCase "ident"           $ ok    atom      "!013-x ; Вот!" (Atom "!013-x")
  , testCase "ident"           $ fail  atom      ".xx"
  , testCase "bool1"           $ ok    bool      "#t "           (Bool True)
  , testCase "bool2"           $ ok    bool      "#f "           (Bool False)
  , testCase "Integer1"        $ ok    integer   "123 "          (Integer 123)
  , testCase "Integer2"        $ ok    integer   "-0666 ; FIXME" (Integer (-666))
  , testCase "string1"         $ ok    string    "\"Wow!\" #| Here you are |# " (String "Wow!")
  , testCase "string2"         $ ok    string    "\"\\\"\\\\\\t\" "  (String "\"\\\t")
  , testCase "string3"         $ fail  string    "\\w "
  , testCase "pair1"           $ ok    pair      "( ) "          Nil
  , testCase "pair1x"          $ ok    pair      "(()) "         (Pair Nil Nil)
  , testCase "pair2"           $ ok    pair      "( x ) "        (Pair (Atom "x") Nil)
  , testCase "pair3"           $ ok    pair      "( -23) "       (Pair (Integer (-23)) Nil)
  , testCase "pair4"           $ ok    pair      "(\"oops\") "   (Pair (String "oops") Nil)
  , testCase "pair5"           $ ok    pair      "(() \"oops\")" (Pair Nil (Pair (String "oops") Nil))
  , testCase "pair6"           $ ok    pair      "(().()) "      (Pair Nil Nil)
  , testCase "pair7"           $ ok    pair      "(().\"oops\")" (Pair Nil (String "oops"))
  , testCase "pair8"           $ ok    pair      "(\"oops\".())" (Pair (String "oops") Nil)
  , testCase "pair9"           $ ok    pair      "( #t . () ) "  (Pair (Bool True) Nil)
  , testCase "pair10"          $ ok    pair      "( () . #f ) "  (Pair Nil (Bool False))
  , testCase "pair11"          $ ok    pair      "(a b c) "      (Pair (Atom "a") (Pair (Atom "b") (Pair (Atom "c") Nil)))
  , testCase "pair12"          $ ok    pair      "(a b . c) "    (Pair (Atom "a") (Pair (Atom "b") (Atom "c")))
  , testCase "pair13"          $ fail  pair      "(a . b c) "
  ]
  where
    ok parser str expected = case parse (parser <* eof) "" str of
      Left e -> assertFailure $ show e
      Right actual -> assertEqual "" expected actual

    fail parser str = case parse (parser <* eof) "" str of
      Left _ -> return ()
      Right x -> assertFailure $ "parser returned: " ++ show x

