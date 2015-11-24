
module SICP.DB
  ( evaluate
  , tests
  ) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict as S

import Pipes
import qualified Pipes.Prelude as P

import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import Data.Set (Set)
import qualified Data.Set as Set

import SICP.LispParser hiding (tests)

import Test.Tasty
import Test.Tasty.HUnit

-- TODO: profiling

-- TODO: rewrite (Producer -> Producer) as Pipe
-- http://stackoverflow.com/questions/22425432/how-to-find-the-end-of-pipe

-- TODO: (from Tasty webhome) Re-organize the project into a library and a program, 
--       so that both the program and the test suite depend on this new library.
--       The library can be declared in the same cabal file.

-------------------------------------------------------------------------------
-- Datatypes
-------------------------------------------------------------------------------

type Frame = Map.Map String Value
type Rule = (Value, Value)

type DBMonad = ReaderT (Set Value, DB) (StateT Int IO)

runDBMonad :: DB -> DBMonad a -> IO a
runDBMonad db m = evalStateT (runReaderT m (Set.empty, db)) 0

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

qeval :: Value -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
qeval (Atom "and" `Pair` q)
  | isJust plq = conjoin $ fromJust plq
  where plq = properList q
qeval (Atom "or" `Pair` q)
  | isJust plq = disjoin $ fromJust plq
  where plq = properList q
qeval (Atom "not" `Pair` (q `Pair` Nil)) = negateQuery q
qeval (Atom "lisp-value"  `Pair` _) = undefined
qeval (Atom "always-true" `Pair` _) = id
qeval q = simpleQuery q

properList :: Value -> Maybe [Value]
properList (x `Pair` xs) = (x:) <$> properList xs
properList Nil = Just []
properList _ = Nothing

evaluate :: [Value] -> Value -> Value -> (x -> Value -> IO x) -> IO x -> (x -> IO b) -> IO b
evaluate db q o f ii ee = runDBMonad (createDB db) $ foldDBMonad (\x y -> liftIO (f x y)) (liftIO ii) (liftIO . ee)

  where

    foldDBMonad :: (x -> Value -> DBMonad x) -> DBMonad x -> (x -> DBMonad b) -> DBMonad b
    foldDBMonad g i e = P.foldM g i e p

    p :: Producer Value DBMonad ()
    p = qeval q (yield Map.empty) >-> P.map (instantiate o)

simpleQuery :: Value -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
simpleQuery q = mapFlattenInterleave (\f -> findAssertions q f >> applyRules q f)

conjoin :: [Value] -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
conjoin = foldr (\a b -> b . qeval a) id

disjoin :: [Value] -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
disjoin = foldr (\a b s -> interleave (qeval a s) (b s)) (const emptyStream)

negateQuery :: Value -> Producer Frame DBMonad () -> Producer Frame DBMonad ()
negateQuery q s = s >-> P.mapM tryQ >-> P.mapFoldable id
  where
    tryQ :: Frame -> DBMonad (Maybe Frame)
    tryQ f = (\x -> if x then Just f else Nothing) `liftM` (P.null . qeval q . yield) f

-------------------------------------------------------------------------------
-- 4.4.4.3 Поиск утверждений с помощью сопоставления с образцом
-------------------------------------------------------------------------------

findAssertions :: Value -> Frame -> Producer Frame DBMonad ()
findAssertions p f = fetchAssertions p >-> P.mapFoldable (\x -> patternMatch p x f)

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

applyRules :: Value -> Frame -> Producer Frame DBMonad ()
applyRules p f = mapFlattenInterleave (applyARule p f) (fetchRules p)

applyARule :: Value -> Frame -> Rule -> Producer Frame DBMonad ()
applyARule p f (c, b) = do
  serial <- lift tick
  let cRenamed = renameVariables c serial
  case unifyMatch p cRenamed f of
    Just ff -> do
      let xn = fixValue cRenamed ff
      alreadySeen <- lift $ liftM (Set.member xn) askZ
      if alreadySeen
        then emptyStream
        else hoist (localAddValue xn) $ qeval (renameVariables b serial) (yield ff)
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

createDB :: [Value] -> DB
createDB = foldr addAssertionOrRule emptyDB

fetch :: Indexable a => (DB -> [a]) -> (DB -> MapStrict.Map String [a]) -> Value -> Producer a DBMonad ()
fetch sf sif p = case index p of
  Nothing -> liftM sf (lift askDB) >>= each
  Just i -> liftM sif (lift askDB) >>= (maybe emptyStream each . MapStrict.lookup i)

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

tests :: [Value] -> TestTree
tests db = testGroup "DB internal"

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
  , testCase "fetchAssertions" $ runDBMonad (createDB db) (P.length $ fetchAssertions Nil) >>= (@?= 47)
  , testCase "fetchRules"      $ runDBMonad (createDB db) (P.length $ fetchRules Nil)      >>= (@?= 22)

  , testCase "properList1" (properList Nil @?= Just [])
  , testCase "properList2" (properList (Atom "x" `Pair` Nil) @?= Just [Atom "x"])
  , testCase "properList3" (properList (Atom "x" `Pair` Atom "b") @?= Nothing)

  ]
  where

    fn n = each [1 :: Int .. n]

    testEqualProducer s expected actual = testCase s $ P.toListM actual >>= (@?= expected)
