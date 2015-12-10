
module SICP.DB
  ( DB
  , compileDB
  , evaluate
  , tests
  ) where

import Data.Maybe
import Data.List
import Data.Foldable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as S

import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import Data.Set (Set)
import qualified Data.Set as Set

import SICP.LispParser hiding (tests)

import Test.Tasty
import Test.Tasty.HUnit

-- TODO: profiling

-------------------------------------------------------------------------------
-- Datatypes
-------------------------------------------------------------------------------

type Frame = Map.Map String Value
type Rule = (Value, Value)

type DBMonad = ReaderT (Set Value, DB) (State Int)

runDBMonad :: DB -> DBMonad a -> a
runDBMonad db m = evalState (runReaderT m (Set.empty, db)) 0

tick :: DBMonad Int
tick = lift $ state $ \s -> (s, s + 1)

askZ :: DBMonad (Set Value)
askZ = liftM fst ask

localAddValue :: Value -> DBMonad a -> DBMonad a
localAddValue v = local f
  where
    f (z, db) = (Set.insert v z, db)

askDB :: DBMonad DB
askDB = liftM snd ask

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
    fixName :: String -> S.State [String] String
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

qeval :: Value -> [Frame] -> DBMonad [Frame]
qeval (Atom "and" `Pair` q)
  | isJust plq = conjoin $ fromJust plq
  where plq = properList q
qeval (Atom "or" `Pair` q)
  | isJust plq = disjoin $ fromJust plq
  where plq = properList q
qeval (Atom "not" `Pair` (q `Pair` Nil)) = negateQuery q
qeval (Atom "lisp-value"  `Pair` _) = undefined
qeval (Atom "always-true" `Pair` _) = return
qeval q = simpleQuery q

properList :: Value -> Maybe [Value]
properList (x `Pair` xs) = (x:) <$> properList xs
properList Nil = Just []
properList _ = Nothing

evaluate :: DB -> Value -> Value -> [Value]
evaluate db query outputPattern = map (instantiate outputPattern) $ runDBMonad db $ qeval query [Map.empty]

simpleQuery :: Value -> [Frame] -> DBMonad [Frame]
simpleQuery q fs = liftM flattenInterleave $ mapM expandOneFrame fs
  where
    expandOneFrame :: Frame -> DBMonad [Frame]
    expandOneFrame f = (++) <$> findAssertions q f <*> applyRules q f

conjoin :: [Value] -> [Frame] -> DBMonad [Frame]
conjoin vs fs = foldlM (flip qeval) fs vs

disjoin :: [Value] -> [Frame] -> DBMonad [Frame]
disjoin = foldr f (const $ return [])
  where
    f :: Value -> ([Frame] -> DBMonad [Frame]) -> [Frame] -> DBMonad [Frame]
    f v g fs = interleave <$> (qeval v fs) <*> (g fs)

negateQuery :: Value -> [Frame] -> DBMonad [Frame]
negateQuery q s = filterM tryQ s
  where
    tryQ :: Frame -> DBMonad (Bool)
    tryQ f = null `fmap` qeval q [f]

-------------------------------------------------------------------------------
-- 4.4.4.3 Поиск утверждений с помощью сопоставления с образцом
-------------------------------------------------------------------------------

findAssertions :: Value -> Frame -> DBMonad [Frame]
findAssertions p f = mapMaybe (\x -> patternMatch p x f) `liftM` (fetchAssertions p)

patternMatch :: Value -> Value -> Frame -> Maybe Frame
patternMatch (Atom ('?':pn)) d f = case Map.lookup pn f of
  Just pnb -> patternMatch pnb d f
  Nothing -> Just $ Map.insert pn d f
patternMatch (Pair pl pr) (Pair dl dr) f = patternMatch pl dl f >>= patternMatch pr dr
patternMatch p d f
  | p == d = Just f
  | otherwise = Nothing

-------------------------------------------------------------------------------
-- 4.4.4.4 Правила и унификация
-------------------------------------------------------------------------------

applyRules :: Value -> Frame -> DBMonad [Frame]
applyRules p f = do
  rs <- fetchRules p
  flattenInterleave `fmap` mapM (applyARule p f) rs

applyARule :: Value -> Frame -> Rule -> DBMonad [Frame]
applyARule p f (c, b) = do
  serial <- tick
  let cRenamed = renameVariables c serial
  case unifyMatch p cRenamed f of
    Just ff -> do
      let xn = fixValue cRenamed ff
      alreadySeen <- liftM (Set.member xn) askZ
      if alreadySeen
        then return []
        else (localAddValue xn) $ qeval (renameVariables b serial) [ff]
    Nothing -> return []

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
  , rules :: [Rule]
  , rulesIndexed :: MapStrict.Map String [Rule]
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

compileDB :: [Value] -> DB
compileDB = foldr addAssertionOrRule emptyDB

fetch :: Indexable a => (DB -> [a]) -> (DB -> MapStrict.Map String [a]) -> Value -> DBMonad [a]
fetch sf sif p = case index p of
  Nothing -> liftM sf askDB
  Just i ->  ((fromMaybe []) . (MapStrict.lookup i) . sif) `fmap` askDB

fetchAssertions :: Value -> DBMonad [Value]
fetchAssertions = fetch assertions assertionsIndexed

fetchRules :: Value -> DBMonad [Rule]
fetchRules = fetch rules rulesIndexed

-------------------------------------------------------------------------------
-- 4.4.4.6 Операции над потоками
-------------------------------------------------------------------------------

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

flattenInterleave :: [[a]] -> [a]
flattenInterleave [] = []
flattenInterleave (x : xs) = interleave x $ flattenInterleave xs

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

tests :: DB -> TestTree
tests db = testGroup "DB internal"

-- streams
  [ testCase "interleave"             $ (interleave [1 :: Int, 2, 3, 4] [10, 20]) @?= [1,10,2,20,3,4]
  , testCase "interleave2"            $ (interleave [1 :: Int, 2, 3, 4] [])       @?= [1,2,3,4]
  , testCase "interleave3"            $ (interleave [] [1 :: Int, 2, 3, 4])       @?= [1,2,3,4]
  , testCase "flatteinInterleave"     $ (flattenInterleave [[1 :: Int .. 3], [1 .. 4], [1 .. 5], [1 .. 6]])
                                                                                  @?= [1,1,2,1,3,2,1,3,2,4,2,3,3,4,4,5,5,6]

  , testCase "properList1" $ properList Nil                        @?= Just []
  , testCase "properList2" $ properList (Atom "x" `Pair` Nil)      @?= Just [Atom "x"]
  , testCase "properList3" $ properList (Atom "x" `Pair` Atom "b") @?= Nothing

-- database
  , testCase "fetchAssertions" $ (length $ runDBMonad db $ fetchAssertions Nil) @?= 47
  , testCase "fetchRules"      $ (length $ runDBMonad db $ fetchRules Nil)      @?= 22
  ]
