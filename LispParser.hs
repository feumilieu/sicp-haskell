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
  , LispParser.space
  , toDoc
  , propValue
  , tests
  ) where

import Data.Char ( isSpace, digitToInt )
import Control.Applicative hiding ( (<|>), many )

import Text.Parsec

import Text.PrettyPrint ( (<+>), (<>) )
import qualified Text.PrettyPrint as PP

import Test.HUnit
import Test.QuickCheck


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
      parsecRead' = fmap (:[]) $ (,) <$> (LispParser.space >> lispExpr) <*> getInput

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

whitespace, lineComment, nestedComment, space :: Stream s m Char => ParsecT s u m ()
whitespace        = satisfy isSpace >> return ()
lineComment       = char ';' >> manyTill anyToken ((oneOf "\n\t" >> return ()) <|> eof ) >> return ()
nestedComment     = try (string "#|") >> inNestedComment
  where
    inNestedComment   = (try (string "|#") >> return ())
      <|> (nestedComment >> inNestedComment)
      <|> (skipMany1 (noneOf "#|") >> inNestedComment)
      <|> (oneOf "#|" >> inNestedComment)
space             = skipMany $ whitespace <|> lineComment <|> nestedComment

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme t          = t <* LispParser.space

idInitialChars, idSubsequentChars :: [Char]
idInitialChars    = "!$%&*/:<=>?^_~"
idSubsequentChars = "+-.@"

lispAtom :: Stream s m Char => ParsecT s u m Value
lispAtom          = Atom <$> (lexeme $ (:) <$> initial <*> many subsequent)
  where
    initial           = letter <|> oneOf idInitialChars
    subsequent        = initial <|> digit <|> oneOf idSubsequentChars

lispBool :: Stream s m Char => ParsecT s u m Value
lispBool          = Bool <$> (lexeme $ (char '#' >> ((char 't' >> return True) <|> (char 'f' >> return False))))

lispInteger :: Stream s m Char => ParsecT s u m Value
lispInteger       = Integer <$> (lexeme $ int)
  where
    makeInt           = foldl (\x y -> x * 10 + y) 0
    sign              = (char '-' >> return negate)
      <|> (char '+' >> return id)
      <|> return id
    int               = sign <*> nat
    nat               = makeInt <$> many1 (fromIntegral . digitToInt <$> digit)

lispString :: Stream s m Char => ParsecT s u m Value
lispString        = String <$> (lexeme $ between (char '\"') (char '\"') stringInternal)
  where
    stringEscapes     = char '\"'
      <|> char '\\'
      <|> '\a' <$ char 'a'
      <|> '\b' <$ char 'b'
      <|> '\t' <$ char 't'
      <|> '\n' <$ char 'n'
      <|> '\r' <$ char 'r'
    stringInternal    = many $ (noneOf "\\\"") <|> (char '\\' >> stringEscapes)

lispPair :: Stream s m Char => ParsecT s u m Value
lispPair          = (lexeme $ char '(') >> inlispPairL
  where
    inlispPairR l     = ((lexeme $ char ')') >> (return $ Pair l Nil))
      <|> ((lexeme $ char '.') >> (Pair l <$> lispExpr) <* (lexeme $ char ')'))
      <|> (Pair l <$> (lispExpr >>= inlispPairR) )
    inlispPairL       = (lexeme $ char ')' >> return Nil)
      <|> (lispExpr >>= inlispPairR)

lispExpr :: Stream s m Char => ParsecT s u m Value
lispExpr          = lispAtom <|> lispBool <|> lispInteger <|> lispString <|> lispPair

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
      genAtom = Atom <$> ((:) <$> genInitChar <*> (listOf genSubsequentChar))

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

tests :: Test
tests = TestList
  [ "whitespace1"     ~: parseOK    whitespace      " "             ()
  , "whitespace2"     ~: parseOK    whitespace      "\n"            ()
  , "lineComment1"    ~: parseOK    lineComment     "; привет \r"   ()
  , "lineComment2"    ~: parseOK    lineComment     "; привет "     ()
  , "nestedComment1"  ~: parseOK    nestedComment   "#||#"          ()
  , "nestedComment2"  ~: parseOK    nestedComment   "#|#||#|#"      ()
  , "nestedComment2"  ~: parseFail  nestedComment   "#|#||#|# "
  , "nestedComment3"  ~: parseFail  nestedComment   "#|"
  , "nestedComment4"  ~: parseFail  nestedComment   "#||"
  , "nestedComment5"  ~: parseFail  nestedComment   "#|#|"
  , "nestedComment6"  ~: parseFail  nestedComment   "#|#||#|"
  , "space"           ~: parseOK    LispParser.space
         "   \n\t #|  Привет, как дела? #|!!!|# |# ; \r ;  Ура!"    ()
  , "ident"           ~: parseOK    lispAtom      "!013-x ; Вот!" (Atom "!013-x")
  , "ident"           ~: parseFail  lispAtom      ".xx"
  , "bool1"           ~: parseOK    lispBool      "#t "           (Bool True)
  , "bool2"           ~: parseOK    lispBool      "#f "           (Bool False)
  , "Integer1"        ~: parseOK    lispInteger   "123 "          (Integer 123)
  , "Integer2"        ~: parseOK    lispInteger   "-0666 ; FIXME" (Integer (-666))
  , "string1"         ~: parseOK    lispString    "\"Wow!\" #| Here you are |# " (String "Wow!")
  , "string2"         ~: parseOK    lispString    "\"\\\"\\\\\\t\" "  (String "\"\\\t")
  , "string3"         ~: parseFail  lispString    "\\w "
  , "pair1"           ~: parseOK    lispPair      "( ) "          Nil
  , "pair1x"          ~: parseOK    lispPair      "(()) "         (Pair Nil Nil)
  , "pair2"           ~: parseOK    lispPair      "( x ) "        (Pair (Atom "x") Nil)
  , "pair3"           ~: parseOK    lispPair      "( -23) "       (Pair (Integer (-23)) Nil)
  , "pair4"           ~: parseOK    lispPair      "(\"oops\") "   (Pair (String "oops") Nil)
  , "pair5"           ~: parseOK    lispPair      "(() \"oops\")" (Pair Nil (Pair (String "oops") Nil))
  , "pair6"           ~: parseOK    lispPair      "(().()) "      (Pair Nil Nil)
  , "pair7"           ~: parseOK    lispPair      "(().\"oops\")" (Pair Nil (String "oops"))
  , "pair8"           ~: parseOK    lispPair      "(\"oops\".())" (Pair (String "oops") Nil)
  , "pair9"           ~: parseOK    lispPair      "( #t . () ) "  (Pair (Bool True) Nil)
  , "pair10"          ~: parseOK    lispPair      "( () . #f ) "  (Pair Nil (Bool False))
  , "pair11"          ~: parseOK    lispPair      "(a b c) "      (Pair (Atom "a") (Pair (Atom "b") (Pair (Atom "c") Nil)))
  , "pair12"          ~: parseOK    lispPair      "(a b . c) "    (Pair (Atom "a") (Pair (Atom "b") (Atom "c")))
  , "pair13"          ~: parseFail  lispPair      "(a . b c) "
  ]
  where
    parseOK parser str expected = test $ case parse (parser <* eof) "" str of
      Left e -> assertFailure $ show e
      Right actual -> assertEqual "" expected actual

    parseFail parser str = test $ case parse (parser <* eof) "" str of
      Left _ -> return ()
      Right x -> assertFailure $ "parser returned: " ++ show x

propValue :: Value -> Bool
propValue x = either (const False) (== x) $ parse lispExpr "" $ show $ toDoc x





