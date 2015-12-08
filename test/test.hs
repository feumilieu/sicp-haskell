{-# LANGUAGE FlexibleContexts #-} -- for parsec 3

import SICP.LispParser
import SICP.DB

import Text.Parsec hiding (space)

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

dbTests :: DB -> TestTree
dbTests db = testGroup "DB"

-- assertions
  [ testCase "case 1" $ (runQuery "?x" "(начальник ?x (Битобор Бен))")
      @?= parseMultiSet "(Хакер Лиза П)  (Фект Пабло Э) (Поправич Дайко)"

  , testCase "case 2" $ (runQuery "?x" "(должность ?x (бухгалтерия . ?y))")
      @?= parseMultiSet "(Крэтчит Роберт) (Скрудж Эбин)"

  , testCase "case 3" $ (runQuery "?x" "(адрес ?x (Сламервилл . ?y))")
      @?= parseMultiSet "(Фиден Кон) (Дум Хьюго) (Битобор Бен)"

  , testCase "case AND" $ (runQuery "(?x ?y)" "(and (начальник ?x (Битобор Бен)) (адрес ?x ?y))")
      @?= parseMultiSet
        "((Поправич Дайко) (Бостон (Бэй Стейт Роуд) 22))\
        \((Фект Пабло Э)    (Кембридж (Эймс Стрит) 3))\
        \((Хакер Лиза П)    (Кембридж (Массачусетс Авеню) 78))"

  , testCase "case NOT" $ (runQuery "(?person ?his-boss ?z2)"
        "(and \
        \(начальник ?person ?his-boss) \
        \(not (должность ?his-boss (компьютеры . ?z1))) \
        \(должность ?his-boss ?z2))")
      @?= parseMultiSet
        "((Фиден Кон) (Уорбак Оливер) (администрация большая шишка))\
        \((Крэтчит Роберт) (Скрудж Эбин) (бухгалтерия главный бухгалтер))\
        \((Скрудж Эбин) (Уорбак Оливер) (администрация большая шишка))\
        \((Битобор Бен) (Уорбак Оливер) (администрация большая шишка))"

-- rules
  , testCase "rule 1" $ (runQuery "?x" "(живет-около ?x (Битобор Бен))")
      @?= parseMultiSet "(Фиден Кон) (Дум Хьюго)"

  , testCase "rule 2 can-replace" $ (runQuery "?x" "(can-replace ?x (Фект Пабло Э))")
      @?= parseMultiSet "(Битобор Бен) (Хакер Лиза П)"

  , testCase "rule 3 independent" $ (runQuery "?x" "(independent ?x)")
      @?= parseMultiSet "(Скрудж Эбин) (Уорбак Оливер) (Битобор Бен)"

  , testCase "rule 4 " $ (runQuery "(?time ?who)" "(совещание ?who (пятница ?time))")
      @?= parseMultiSet "(13 администрация)"

  , testCase "rule 5" $ (runQuery "?day-and-time" "(время-совещания (Хакер Лиза П) ?day-and-time)")
      @?= parseMultiSet "(среда 16) (среда 15)"

  , testCase "rule 6" $ (runQuery "?time" "(время-совещания (Хакер Лиза П) (среда ?time))")
      @?= parseMultiSet "16 15"

  , testCase "rule 7" $ (runQuery "(?p1 ?p2)" "(живет-около ?p1 ?p2)")
      @?= parseMultiSet
        "((Фиден Кон) (Дум Хьюго))\
        \((Фиден Кон) (Битобор Бен))\
        \((Дум Хьюго) (Фиден Кон))\
        \((Дум Хьюго) (Битобор Бен))\
        \((Хакер Лиза П) (Фект Пабло Э))\
        \((Фект Пабло Э) (Хакер Лиза П))\
        \((Битобор Бен) (Фиден Кон))\
        \((Битобор Бен) (Дум Хьюго))"

  , testCase "rule 8 append-to-form" $ (runQuery "?z" "(append-to-form (a b) (c d) ?z)")
      @?= parseMultiSet "(a b c d)"

  , testCase "rule 9 append-to-form" $ (runQuery "?y" "(append-to-form (a b) ?y (a b c d))")
      @?= parseMultiSet "(c d)"

  , testCase "rule 10 append-to-form" $ (runQuery "(?x ?y)" "(append-to-form ?x ?y (a b c d))")
      @?= parseMultiSet
        "((a b c d) ())\
        \(() (a b c d))\
        \((a) (b c d))\
        \((a b) (c d))\
        \((a b c) (d))"

  , testCase "rule 11 last-pair" $ (runQuery "?x" "(last-pair (3) ?x)")
      @?= parseMultiSet "3"

  , testCase "rule 12 last-pair" $ (runQuery "?x" "(last-pair (1 2 3) ?x)")
      @?= parseMultiSet "3"

  , testCase "rule 13 last-pair" $ (runQuery "?x" "(last-pair (2 ?x) (3))")
      @?= parseMultiSet "(3)"

  , testCase "rule 14 next-to" $ (runQuery "(?x ?y)" "(?x next-to ?y in (1 (2 3) 4))")
      @?= parseMultiSet "((2 3) 4) (1 (2 3))"

  , testCase "rule 15 next-to" $ (runQuery "?x" "(?x next-to 1 in (2 1 3 1))")
      @?=  parseMultiSet "2 3"

  , testCase "rule 16" $ (runQuery "?x" "(подчиняется (Битобор Бен) ?x)")
      @?=  parseMultiSet "(Уорбак Оливер)"
  , testCase "rule 17" $ (runQuery "?x" "(подчиняется1 (Битобор Бен) ?x)")
      @?=  parseMultiSet "(Уорбак Оливер)"

  , testCase "rule 18 reverse" $ (runQuery "?x" "(reverse () ?x)")
      @?= parseMultiSet "()"
  , testCase "rule 19 reverse" $ (runQuery "?x" "(reverse ?x ())")
      @?= parseMultiSet "()"
  , testCase "rule 20 reverse" $ (runQuery "?x" "(reverse (a) ?x)")
      @?= parseMultiSet "(a)"
  , testCase "rule 21 reverse" $ (runQuery "?x" "(reverse ?x (a))")
      @?= parseMultiSet "(a)"
  , testCase "rule 22 reverse" $ (runQuery "?x" "(reverse (a b c d) ?x)")
      @?= parseMultiSet "(d c b a)"
  , testCase "rule 23 reverse" $ (runQuery "?x" "(reverse ?x (a b c d))")
      @?= parseMultiSet "(d c b a)"
  , testCase "rule 24 reverse" $ (runQuery "?x" "(reverse (a b c d e) ?x)")
      @?= parseMultiSet "(e d c b a)"
  , testCase "rule 25 reverse" $ (runQuery "?x" "(reverse ?x (a b c d e))")
      @?= parseMultiSet "(e d c b a)"

-- lisp-value
{-
  , test $ runQuery "(?name ?y ?x)"
        "(and (зарплата (Битобор Бен) ?x)\
        \(зарплата ?name ?y) (lisp-value < ?y ?x))" >>=
      (@?=  "caseLV1" (parseMultiSet
        "((Фиден Кон)      25000 60000)\
        \((Крэтчит Роберт) 18000 60000)\
        \((Дум Хьюго)      30000 60000)\
        \((Поправич Дайко) 25000 60000)\
        \((Фект Пабло Э)   35000 60000)\
        \((Хакер Лиза П)   40000 60000)")

  , test $ runQuery "(?x ?sx ?sy)"
        "(and\
        \(can-replace ?x ?y)\
        \(зарплата ?x ?sx)\
        \(зарплата ?y ?sy)\
        \(lisp-value > ?sy ?sx))" >>=
      (@?=  "caseLV2" (parseMultiSet
        "((Фиден Кон) 25000 150000) ((Фект Пабло Э) 35000 40000)")
-}

    ]
    where

        runQuery :: String -> String -> MultiSet Value
        runQuery o q = foldl (flip MultiSet.insert) MultiSet.empty $ evaluate db (parseExpr q) (parseExpr o)

        parseExpr s = case parse (space >> expr <* eof) "" s of
          Left e -> error $ show e
          Right x -> x

        parseMultiSet :: String -> MultiSet Value
        parseMultiSet s = case parse (space >> many expr <* eof) "" s of
          Left e -> error $ show e
          Right x -> MultiSet.fromList x

propValue' :: Value -> Bool
propValue' x = either (const False) (== x) $ parse expr "" $ show $ toDoc x

propValue :: TestTree
propValue = QC.testProperty "LispParser QuickCheck" propValue'

main :: IO ()
main = do
    edb <- fmap compileDB `liftM` parseFile "data.txt"
    case edb of
        Left e -> print e
        Right db -> defaultMain $ testGroup "Tests" [ SICP.LispParser.tests, propValue, SICP.DB.tests db, dbTests db ]
