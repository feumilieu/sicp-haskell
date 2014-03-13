{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-} -- for parsec 3

module DB
  ( DB
  , dbParser
  , dbParseFile

  , DB.evaluate

  , tests
  ) where

import Data.Maybe
import Data.List
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.State as S
import Control.Monad.Reader

import Pipes
import qualified Pipes.Prelude as P

import Text.Parsec

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import Data.Set (Set)
import qualified Data.Set as Set

import Test.HUnit

import LispParser hiding (tests)
import qualified Data.Text.IO as T
import Text.Parsec.Text ()

-- TODO: profiling
-- TODO: lint

-- TODO: combine two levels of errors (IOError and ParseError) into one in dbParseFile
-- TODO: introduce function [Value] -> DB, remove parser functions
-- TODO: make `evaluate` more generic.  How?

-------------------------------------------------------------------------------
-- Datatypes
-------------------------------------------------------------------------------

type Frame = Map.Map String Value
type Rule = (Value, Value)

type DBMonad = ReaderT DB (StateT Int IO)

runDBMonad :: DB -> DBMonad a -> IO a
runDBMonad db m = evalStateT (runReaderT m db) 0

tick :: DBMonad Int
tick = state $ \s -> (s, s + 1)

-------------------------------------------------------------------------------
-- 4.4.4.1 Управляющий цикл и конкретизация
-------------------------------------------------------------------------------

instantiate :: Value -> Frame -> Value
instantiate e frame = copy e
  where
    copy x@(Atom ('?':xn)) = case Map.lookup xn frame of
      Just xv -> copy xv
      Nothing -> x
    copy (Pair l r) = Pair (copy l) (copy r)
    copy x = x

-------------------------------------------------------------------------------
-- 4.4.4.2 Вычислитель
-------------------------------------------------------------------------------

fixValue' :: Value -> S.State [String] Value
fixValue' (Atom ('?':n)) = Atom <$> fixName n
  where
    fixName :: String -> S.State [String] (String)
    fixName name = do
      names <- get
      case elemIndex name names of
        Nothing -> do
          put $ name : names
          return $ show $ length names
        Just i -> return $ show i
fixValue' (a `Pair` b) = Pair <$> fixValue' a <*> fixValue' b
fixValue' x = return x

fixValue :: Value -> Frame -> Value
fixValue v f = evalState (fixValue' $ instantiate v f) []

-------------------------------------------------------------------------------

qeval :: Set Value -> Value -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
qeval z (Atom "and" `Pair` q)
  | isJust plq = conjoin z $ fromJust plq
  where plq = properList q
qeval z (Atom "or" `Pair` q)
  | isJust plq = disjoin z $ fromJust plq
  where plq = properList q
qeval z (Atom "not" `Pair` (q `Pair` Nil)) = negateQuery z q
qeval _ (Atom "lisp-value"  `Pair` _) = undefined
qeval _ (Atom "always-true" `Pair` _) = id
qeval z q = simpleQuery z q

properList :: Value -> Maybe [Value]
properList (x `Pair` xs) = (x:) <$> properList xs
properList Nil = Just []
properList _ = Nothing

evaluate :: DB -> Value -> Value -> (Value -> IO ()) -> IO ()
evaluate db q o f = runDBMonad db $ runEffect $ qeval Set.empty q (yield Map.empty) >-> P.map (instantiate o) >-> P.mapM (liftIO . f) >-> P.drain

simpleQuery :: Set Value -> Value -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
simpleQuery z q = mapFlattenInterleave (\f -> findAssertions q f >> applyRules z q f)

conjoin :: Set Value -> [Value] -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
conjoin z = foldr (\a b -> b . qeval z a) id

disjoin :: Set Value -> [Value] -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
disjoin z = foldr (\a b s -> interleave (qeval z a s) (b s)) (const emptyStream)

negateQuery :: Set Value -> Value -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
negateQuery z q = mapFlattenInterleave tryQ
  where
    tryQ f = do
      b <- lift $ P.null $ qeval z q $ yield f
      if b then yield f else emptyStream

-------------------------------------------------------------------------------
-- 4.4.4.3 Поиск утверждений с помощью сопоставления с образцом
-------------------------------------------------------------------------------

findAssertions :: Value -> Frame -> Producer Frame DBMonad ()
findAssertions p f = mapFlattenInterleave (checkAnAssertion p f) (fetchAssertions p)

checkAnAssertion :: Value -> Frame -> Value -> Producer Frame DBMonad ()
checkAnAssertion p f a = case patternMatch p a f of
  Just f' -> yield f'
  Nothing -> emptyStream

patternMatch :: Value -> Value -> Frame -> Maybe Frame
patternMatch (Atom ('?':pn)) d f = case Map.lookup pn f of
  Just pnb -> patternMatch pnb d f
  Nothing -> Just $ Map.insert pn d f
patternMatch (Pair pl pr) (Pair dl dr) f = patternMatch pl dl f >>= patternMatch pr dr
patternMatch p d f = if p == d then Just f else Nothing

-------------------------------------------------------------------------------
-- 4.4.4.4 Правила и унификация
-------------------------------------------------------------------------------

applyRules :: Set Value -> Value -> Frame -> Producer Frame DBMonad ()
applyRules z p f = mapFlattenInterleave (applyARule z p f) (fetchRules p)

applyARule :: Set Value -> Value -> Frame -> Rule -> Producer Frame DBMonad ()
applyARule z p f (c, b) = do
  serial <- lift tick
  let cRenamed = renameVariables c serial
  case unifyMatch p cRenamed f of
    Just ff -> do
      let xn = fixValue cRenamed ff
      if Set.member xn z
        then emptyStream
        else qeval (Set.insert xn z) (renameVariables b serial) $ yield ff
    Nothing -> emptyStream

renameVariables :: Value -> Int -> Value
renameVariables e serial = treeWalk e
  where
    treeWalk (Atom ('?':var)) = Atom $ "?" ++ var ++ "-" ++ show serial
    treeWalk (Pair l r) = Pair (treeWalk l) (treeWalk r)
    treeWalk x = x

unifyMatch :: Value -> Value -> Frame -> Maybe Frame
unifyMatch v1 v2
  | v1 == v2 = Just
  | otherwise = unifyMatch' v1 v2
    where
      unifyMatch' (Atom ('?':n1)) vv2 f = extendIfPossible n1 vv2 f
      unifyMatch' vv1 (Atom ('?':n2)) f = extendIfPossible n2 vv1 f
      unifyMatch' (Pair l1 r1) (Pair l2 r2) f = unifyMatch l1 l2 f >>= unifyMatch r1 r2
      unifyMatch' _ _ _ = Nothing

extendIfPossible :: String -> Value -> Frame -> Maybe Frame
extendIfPossible var val f =
  case Map.lookup var f of
    Just b -> unifyMatch b val f
    Nothing -> case val of
      (Atom ('?':valn)) -> case Map.lookup valn f of
        Just b2 -> unifyMatch (Atom ('?':var)) b2 f
        Nothing -> Just $ Map.insert var val f
      _ -> if dependsOn val var f then Nothing else Just $ Map.insert var val f

dependsOn :: Value -> String -> Frame -> Bool
dependsOn e var frame = treeWalk e
  where
    treeWalk ee = case ee of
        (Atom ('?':een)) -> (een == var) ||
          (case Map.lookup een frame of
            Just eenv -> treeWalk eenv
            Nothing -> False)
        _ -> case ee of
          (Pair l r) -> treeWalk l || treeWalk r
          _ -> False

-------------------------------------------------------------------------------
-- 4.4.4.5 Ведение базы данных
-------------------------------------------------------------------------------

data DB = DB
  { assertions :: [Value]
  , assertionsIndexed :: MapStrict.Map String [Value]
  , rules :: [(Value, Value)]
  , rulesIndexed :: MapStrict.Map String [(Value, Value)]
  }

emptyDB :: DB
emptyDB = DB
  { assertions = []
  , assertionsIndexed = MapStrict.empty
  , rules = []
  , rulesIndexed = MapStrict.empty
  }

class Indexable a where
  index :: a -> Maybe String

instance Indexable Value where
  index (Atom ('?':_) `Pair` _) = Nothing
  index (Atom x `Pair` _) = Just x
  index _ = Nothing

-- instance Indexable Rule where -- TODO?: TypeSynonymInstances, FlexibleInstances
instance Indexable a => Indexable (a, b) where
  index (c, _) = index c

storeInIndex :: Indexable a => a -> MapStrict.Map String [a] -> MapStrict.Map String [a]
storeInIndex a db =
  case index a of
    Nothing -> db
    Just ai -> case MapStrict.lookup ai db of
      Nothing -> MapStrict.insert ai [a] db
      Just as -> MapStrict.insert ai (a:as) db

addAssertionOrRule :: Value -> DB -> DB
addAssertionOrRule x = case x of
  (Atom "rule" `Pair` (conclusion `Pair` (body `Pair` Nil))) -> addRule (conclusion, body)
  (Atom "rule" `Pair` (conclusion `Pair` Nil)) -> addRule (conclusion, Atom "always-true" `Pair` Nil)
  a -> addAssertion a
  where
    addAssertion a db = db {
      assertions = a : assertions db,
      assertionsIndexed = storeInIndex a $ assertionsIndexed db
    }
    addRule r db = db {
      rules = r : rules db,
      rulesIndexed = storeInIndex r $ rulesIndexed db
    }

dbParser :: Stream s m Char => ParsecT s u m DB
dbParser = foldr addAssertionOrRule emptyDB `liftM` (LispParser.space >> many lispExpr <* eof)

dbParseFile :: String -> IO (Either ParseError DB)
dbParseFile fname = runP dbParser () fname `liftM` T.readFile fname

fetch :: Indexable a => (DB -> [a]) -> (DB -> MapStrict.Map String [a]) -> Value -> Producer a DBMonad ()
fetch sf sif p = case index p of
  Nothing -> liftM sf (lift ask) >>= each
  Just i -> liftM sif (lift ask) >>= (maybe emptyStream each . MapStrict.lookup i)

fetchAssertions :: Value -> Producer Value DBMonad ()
fetchAssertions = fetch assertions assertionsIndexed

fetchRules :: Value -> Producer Rule DBMonad ()
fetchRules = fetch rules rulesIndexed

-------------------------------------------------------------------------------
-- 4.4.4.6 Операции над потоками
-------------------------------------------------------------------------------

interleave :: Monad m => Producer a m () -> Producer a m () -> Producer a m ()
interleave a b = do
  n <- lift $ next a
  case n of
    Left () -> b
    Right (x, a') -> do
      yield x
      interleave b a'

flattenInterleave :: Monad m => Producer (Producer a m ()) m () -> Producer a m ()
flattenInterleave pp = do
  n <- lift $ next pp
  case n of
    Left () -> lift $ return ()
    Right (x, pp') -> interleave x (flattenInterleave pp')

mapFlattenInterleave :: Monad m => (a -> Producer b m ()) -> Producer a m () -> Producer b m ()
mapFlattenInterleave f a = flattenInterleave $ a >-> P.map f

emptyStream :: Monad m => Producer a m ()
emptyStream = return ()

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

tests :: DB -> Test
tests db = TestList

-- streams

  [ testEqualProducer "interleave" [1,10,2,20,3,4] $
      interleave (each [1 :: Int, 2, 3, 4]) (each [10, 20])
  , testEqualProducer "interleave2" [1,2,3,4] $
      interleave (each [1 :: Int, 2, 3, 4]) emptyStream
  , testEqualProducer "interleave3" [1,2,3,4] $
      interleave emptyStream (each [1 :: Int, 2, 3, 4])
  , testEqualProducer "flatteinInterleave" [1,1,2,1,3,2,1,3,2,4,2,3,3,4,4,5,5,6] $
      flattenInterleave (each [3, 4, 5, 6] >-> P.map fn)
  , testEqualProducer "mapFlatteinInterleave" [1,1,2,1,3,2,1,3,2,4,2,3,3,4,4,5,5,6] $
      mapFlattenInterleave fn (each [3, 4, 5, 6])
  , testEqualProducer "append" [3,4,5,6,10,11,12] $
      each [3 :: Int, 4, 5, 6] >> each [10, 11, 12]

-- database

  , test $ runDBMonad db (P.length $ fetchAssertions Nil) >>= assertEqual "fetchAssertions" 47
  , test $ runDBMonad db (P.length $ fetchRules Nil)      >>= assertEqual "fetchRules" 22

-- assertions

  , test $ runQuery "?x" "(начальник ?x (Битобор Бен))" >>=
      assertEqual "case1" (parseMultiSet "(Хакер Лиза П)  (Фект Пабло Э) (Поправич Дайко)")

  , test $ runQuery "?x" "(должность ?x (бухгалтерия . ?y))" >>=
      assertEqual "case2" (parseMultiSet "(Крэтчит Роберт) (Скрудж Эбин)")

  , test $ runQuery "?x" "(адрес ?x (Сламервилл . ?y))" >>=
      assertEqual "case3" (parseMultiSet "(Фиден Кон) (Дум Хьюго) (Битобор Бен)")

  , test $ runQuery "(?x ?y)" "(and (начальник ?x (Битобор Бен)) (адрес ?x ?y))" >>=
      assertEqual "case4" (parseMultiSet
        "((Поправич Дайко) (Бостон (Бэй Стейт Роуд) 22))\
        \((Фект Пабло Э)    (Кембридж (Эймс Стрит) 3))\
        \((Хакер Лиза П)    (Кембридж (Массачусетс Авеню) 78))")

  , test $ runQuery "(?person ?his-boss ?z2)"
        "(and \
        \(начальник ?person ?his-boss) \
        \(not (должность ?his-boss (компьютеры . ?z1))) \
        \(должность ?his-boss ?z2))" >>=
      assertEqual "case5" (parseMultiSet
        "((Фиден Кон) (Уорбак Оливер) (администрация большая шишка))\
        \((Крэтчит Роберт) (Скрудж Эбин) (бухгалтерия главный бухгалтер))\
        \((Скрудж Эбин) (Уорбак Оливер) (администрация большая шишка))\
        \((Битобор Бен) (Уорбак Оливер) (администрация большая шишка))")

-- rules

  , test $ runQuery "?x" "(живет-около ?x (Битобор Бен))" >>=
      assertEqual "caseR1" (parseMultiSet "(Фиден Кон) (Дум Хьюго)")

  , test $ runQuery "?x" "(can-replace ?x (Фект Пабло Э))" >>=
      assertEqual "caseR2" (parseMultiSet "(Битобор Бен) (Хакер Лиза П)")

  , test $ runQuery "?x" "(independent ?x)" >>=
      assertEqual "caseR3" (parseMultiSet "(Скрудж Эбин) (Уорбак Оливер) (Битобор Бен)")

  , test $ runQuery "(?time ?who)" "(совещание ?who (пятница ?time))" >>=
      assertEqual "caseR4" (parseMultiSet "(13 администрация)")

  , test $ runQuery "?day-and-time" "(время-совещания (Хакер Лиза П) ?day-and-time)" >>=
      assertEqual "caseR5" (parseMultiSet "(среда 16) (среда 15)")

  , test $ runQuery "?time" "(время-совещания (Хакер Лиза П) (среда ?time))" >>=
      assertEqual "caseR6" (parseMultiSet "16 15")

  , test $ runQuery "(?p1 ?p2)" "(живет-около ?p1 ?p2)" >>=
      assertEqual "caseR7" (parseMultiSet
        "((Фиден Кон) (Дум Хьюго))\
        \((Фиден Кон) (Битобор Бен))\
        \((Дум Хьюго) (Фиден Кон))\
        \((Дум Хьюго) (Битобор Бен))\
        \((Хакер Лиза П) (Фект Пабло Э))\
        \((Фект Пабло Э) (Хакер Лиза П))\
        \((Битобор Бен) (Фиден Кон))\
        \((Битобор Бен) (Дум Хьюго))")

  , test $ runQuery "?z" "(append-to-form (a b) (c d) ?z)" >>=
      assertEqual "caseR8" (parseMultiSet "(a b c d)")

  , test $ runQuery "?y" "(append-to-form (a b) ?y (a b c d))" >>=
      assertEqual "caseR9" (parseMultiSet "(c d)")

  , test $ runQuery "(?x ?y)" "(append-to-form ?x ?y (a b c d))" >>=
      assertEqual "caseR10" (parseMultiSet
        "((a b c d) ())\
        \(() (a b c d))\
        \((a) (b c d))\
        \((a b) (c d))\
        \((a b c) (d))")

  , test $ runQuery "?x" "(last-pair (3) ?x)" >>=
      assertEqual "caseR11" (parseMultiSet "3")

  , test $ runQuery "?x" "(last-pair (1 2 3) ?x)" >>=
      assertEqual "caseR12" (parseMultiSet "3")

  , test $ runQuery "?x" "(last-pair (2 ?x) (3))" >>=
      assertEqual "caseR13" (parseMultiSet "(3)")

  , test $ runQuery "(?x ?y)" "(?x next-to ?y in (1 (2 3) 4))" >>=
      assertEqual "caseR14" (parseMultiSet "((2 3) 4) (1 (2 3))")

  , test $ runQuery "?x" "(?x next-to 1 in (2 1 3 1))" >>=
      assertEqual "caseR15" (parseMultiSet "2 3")

  , test $ assertEqual "properList1" (properList Nil) (Just [])
  , test $ assertEqual "properList2" (properList (Atom "x" `Pair` Nil)) (Just [Atom "x"])
  , test $ assertEqual "properList3" (properList (Atom "x" `Pair` Atom "b")) Nothing

  , test $ runQuery "?x" "(подчиняется (Битобор Бен) ?x)" >>=
      assertEqual "caseR16" (parseMultiSet "(Уорбак Оливер)")
  , test $ runQuery "?x" "(подчиняется1 (Битобор Бен) ?x)" >>=
      assertEqual "caseR17" (parseMultiSet "(Уорбак Оливер)")

  , test $ runQuery "?x" "(reverse () ?x)" >>=
      assertEqual "reverse1" (parseMultiSet "()")
  , test $ runQuery "?x" "(reverse ?x ())" >>=
      assertEqual "reverse2" (parseMultiSet "()")
  , test $ runQuery "?x" "(reverse (a) ?x)" >>=
      assertEqual "reverse3" (parseMultiSet "(a)")
  , test $ runQuery "?x" "(reverse ?x (a))" >>=
      assertEqual "reverse4" (parseMultiSet "(a)")
  , test $ runQuery "?x" "(reverse (a b c d) ?x)" >>=
      assertEqual "reverse5" (parseMultiSet "(d c b a)")
  , test $ runQuery "?x" "(reverse ?x (a b c d))" >>=
      assertEqual "reverse6" (parseMultiSet "(d c b a)")
  , test $ runQuery "?x" "(reverse (a b c d e) ?x)" >>=
      assertEqual "reverse7" (parseMultiSet "(e d c b a)")
  , test $ runQuery "?x" "(reverse ?x (a b c d e))" >>=
      assertEqual "reverse8" (parseMultiSet "(e d c b a)")

-- lisp-value
{-
  , test $ runQuery "(?name ?y ?x)"
        "(and (зарплата (Битобор Бен) ?x)\
        \(зарплата ?name ?y) (lisp-value < ?y ?x))" >>=
      assertEqual "caseLV1" (parseMultiSet
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
      assertEqual "caseLV2" (parseMultiSet
        "((Фиден Кон) 25000 150000) ((Фект Пабло Э) 35000 40000)")
-}
  ]
  where

    fn n = each [1 :: Int .. n]

    testEqualProducer s expected actual = test $ P.toListM actual >>= assertEqual s expected

    toMultiSet :: (Ord a, Monad m) => Producer a m () -> m (MultiSet a)
    toMultiSet = P.fold (flip MultiSet.insert) MultiSet.empty id

    runQuery :: String -> String -> IO (MultiSet Value)
    runQuery o q = (MultiSet.map $ instantiate $ read o) `liftM` (runDBMonad db $ toMultiSet $ qeval Set.empty (read q) $ yield Map.empty)

    parseMultiSet :: String -> MultiSet Value
    parseMultiSet s = case parse (LispParser.space >> many lispExpr <* eof) "" s of
      Left e -> error $ show e
      Right x -> MultiSet.fromList x

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

{-
query :: Value
-- query = read "(зарплата ?x ?y)"
query = read "(?x ?y ?z)"
-- query = read "?x"

output :: Value
-- output = read "?x"
output = query

printProducer :: DB -> (a -> String) -> Producer a DBMonad () -> IO ()
printProducer db f p = runDBMonad db $ runEffect $ p >-> P.map f >-> P.stdoutLn

main :: IO ()
main = do

--  putStrLn "----------------------------------------"

--  _ <- runStateT (runEffect $ readDB' >-> P.map show >-> P.stdoutLn) 0

--  s <- return (qeval query $ yield initialFrame)

--  putStrLn "Frame stream:"
--  runEffect $ s >-> P.map show >-> P.stdoutLn

--  putStrLn "Output:"
--  _ <- runStateT (runEffect $ s >-> P.map (show . (extend output)) >-> P.stdoutLn) 0

  edb <- dbParseFile "data.txt"
  case edb of
    Right db -> do

      -- putStrLn "----------------------------------------"
      -- void $ runTestTT $ tests db

      putStrLn "evaluate -------------------------------"
      evaluate db query output print

      putStrLn "fetchAssertions ------------------------"
      printProducer db show $ fetchAssertions query

      -- putStrLn "findAssertioins ?x ----------------------------------------"
      -- printProducer db show (findAssertions (read "?x") Map.empty)

      -- putStrLn "fetchRules ?x ----------------------------------------"
      -- printProducer db show (fetchRules (read "?x"))

      -- putStrLn "ApplyRules ?x ----------------------------------------"
      -- printProducer db show (applyRules (read "?x") Map.empty)

      -- putStrLn "qeval ?x ----------------------------------------"
      -- printProducer db ((++ "\n") . show . (instantiate $ read "?x")) (simpleQuery (read "?x") (each [Map.empty]))

      -- putStrLn "qeval ?x ----------------------------------------"
      -- printProducer db show (qeval (read "?x") (each [Map.empty]))

      putStrLn "----------------------------------------"

    Left e -> print e

-}

