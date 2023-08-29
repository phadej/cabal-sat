module Main (main) where

import Control.Applicative              (liftA2)
import Control.Monad                    (forM_)
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Coerce                      (coerce)
import Data.Foldable                    (for_, toList, traverse_)
import Data.IntMap.Strict               (IntMap)
import Data.List                        (intercalate, tails)
import GHC.Generics                     (Generic)
import System.Environment               (getArgs)
import Text.Printf                      (printf)

import qualified Data.IntMap.Strict as IntMap

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        "decode" : _    -> decodeDIMACS
        "gen-cabal" : _ -> generateCabal
        "dec-cabal" : _ -> decodeCabal
        _               -> generateDIMACS

problem :: (Int, [[Int]], Model Int)
problem = runSAT $ do
    m <- sudokuModel
    sudokuValues m initValues
    sudokuRules m
    return m

generateDIMACS :: IO ()
generateDIMACS = do
    putStr $ unlines $ map ("c " ++) $ lines $ render initValues

    let (nVars, clauses, _) = problem

    printf "p cnf %d %d\n" nVars (length clauses)
    forM_ clauses $ \clause -> do
        putStrLn $ unwords (map show (clause ++ [0]))

generateCabal :: IO ()
generateCabal = do
    let (nVars, clauses, _) = problem

    -- package header
    putStr $ unlines
        [ "cabal-version: 3.0"
        , "name: sudoku"
        , "version: 0"
        ]

    -- flags
    forM_ [1..nVars] $ \var -> do
        putStr $ unlines
            [ "flag " ++ show var
            , "  manual: False"
            , "  default: False"
            ]

    -- library stanza
    putStr $ unlines
        [ "library"
        ]

    let flagValue :: Int -> [Char]
        flagValue l = printf "%sflag(%d)" (if (l > 0) then "!" else "") (abs l)

    forM_ clauses $ \clause -> do
        putStrLn $ unlines
            [ "  if " ++ intercalate " && " (map flagValue clause)
            , "    build-depends: unsatisfiable <0"
            ]

decodeDIMACS :: IO ()
decodeDIMACS = do
    let assignment :: [Int]
        assignment = map read $ words
            "-1 2 -3 -4 5 -6 -7 -8 -9 -10 -11 12 -13 -14 15 -16 -17 -18 -19 20 -21 -22 23 -24 25 -26 -27 -28 -29 30 -31 -32 33 -34 -35 -36 -37 38 -39 -40 -41 -42 43 -44 -45 -46 -47 48 -49 -50 51 -52 -53 -54 -55 56 -57 58 -59 -60 61 -62 -63 -64"

        amap :: IntMap Bool
        amap = IntMap.fromList
            [ (abs l, l > 0)
            | l <- assignment
            ]

    let model :: Model Int
        (_, _, model) = problem

    let bmodel :: Model Bool
        bmodel = fmap (\i -> IntMap.findWithDefault False i amap) model

    putStr $ render $ decode bmodel

decodeCabal :: IO ()
decodeCabal = do
    let assignment :: [Int]
        assignment = map (read . filter (/= '+')) $ words
            "-1 -10 -11 +12 -13 -14 +15 -16 -17 -18 -19 +2 +20 -21 -22 +23 -24 +25 -26 -27 -28 -29 -3 +30 -31 -32 +33 -34 -35 -36 -37 +38 -39 -4 -40 -41 -42 +43 -44 -45 -46 -47 +48 -49 +5 -50 +51 -52 -53 -54 -55 +56 -57 +58 -59 -6 -60 +61 -62 -63 -64 -7 -8 -9"

        amap :: IntMap Bool
        amap = IntMap.fromList
            [ (abs l, l > 0)
            | l <- assignment
            ]

    let model :: Model Int
        (_, _, model) = problem

    let bmodel :: Model Bool
        bmodel = fmap (\i -> IntMap.findWithDefault False i amap) model

    putStr $ render $ decode bmodel

-------------------------------------------------------------------------------
-- Initial values
-------------------------------------------------------------------------------

initValues :: Four (Four Int)
initValues = F4
    -- From https://en.wikipedia.org/w/index.php?title=Sudoku&oldid=543290082
    (F4 0 1 4 0)
    (F4 0 3 0 2)
    (F4 0 0 3 0)
    (F4 0 0 0 0)

-------------------------------------------------------------------------------
-- Duo
-------------------------------------------------------------------------------

data Duo a = T a a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Applicative Duo where
    pure x = T x x
    T f g <*> T x y = T (f x) (g y)

newtype Four a = N { unN :: Duo (Duo a) }
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Four where
    pure x = N (pure (pure x))
    N f <*> N x = N (liftA2 (<*>) f x)

pattern F4 :: a -> a -> a -> a -> Four a
pattern F4 a b c d = N (T (T a b) (T c d))
{-# COMPLETE F4 #-}

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

render :: Four (Four Int) -> String
render sol = unlines $ renderGroups top divider bottom $ fmap renderLine sol
  where
    top     = bar "┌" "─────" "┬" "┐"
    divider = bar "├" "─────" "┼" "┤"
    bottom  = bar "└" "─────" "┴" "┘"

    bar begin fill middle end = begin ++ fill ++ middle ++ fill ++ end

renderLine :: Four Int -> String
renderLine sol = unwords $ renderGroups "│" "│" "│" $ fmap showN sol
  where
    showN n | 1 <= n && n <= 4 = show n
            | otherwise        = " "

renderGroups :: a -> a -> a -> Four a -> [a]
renderGroups begin middle end (N (T xs ys)) =
    [begin] ++ toList xs ++ [middle] ++ toList ys ++ [end]

-------------------------------------------------------------------------------
-- Sudoku model
-------------------------------------------------------------------------------

-- | Model is nine rows of nine columns of nine bits.
newtype Model a = M (Four (Four (Four a)))
  deriving (Show, Functor, Foldable, Traversable)

emptyModel :: Model ()
emptyModel = M $ pure $ pure $ pure ()

decode :: Model Bool -> Four (Four Int)
decode (M m) = fmap (fmap f) m where
    f :: Four Bool -> Int
    f (F4 X _ _ _) = 1
    f (F4 _ X _ _) = 2
    f (F4 _ _ X _) = 3
    f (F4 _ _ _ X) = 4
    f _ = 0

pattern X :: Bool
pattern X = True

-------------------------------------------------------------------------------
-- SAT rules
-------------------------------------------------------------------------------

-- | Populate model with the literals.
sudokuModel :: SAT s (Model (Lit s))
sudokuModel = traverse (\_ -> newLit) emptyModel

-- | Sudoku rules.
--
-- Add constraints of the puzzle.
sudokuRules :: Model (Lit s) -> SAT s ()
sudokuRules model = do
    -- each "digit" is 1..9
    -- we encode digits using 9 bits.
    -- exactly one, i.e. at most one and and least one have to set.
    forDigit_ model $ \d -> do
        let lits = toList d
        atMostOne lits
        atLeastOne lits

    -- With above digit encoding the sudoku rules are easy to encode:
    -- For each row we should have at least one 1, at least one 2, ... 9
    -- And similarly for columns and subsquares.
    --
    -- If we also require that each row, column and subsquare has at most one 1..9
    -- the given problem becomes trivial, as is solved by initial unit propagation.

    -- each row
    forRow_ model $ \block -> do
        let block' = sequenceA block
        for_ block' $ \d -> do
            let lits = toList d
            atLeastOne lits
            atMostOne lits

     -- each column
    forColumn_ model $ \block -> do
        let block' = sequenceA block
        for_ block' $ \d -> do
            let lits = toList d
            atLeastOne lits
            atMostOne lits

    -- each subsquare
    forSubSq_ model $ \block -> do
        let block' = sequenceA block
        for_ block' $ \d -> do
            let lits = toList d
            atLeastOne lits
            atMostOne lits

forDigit_ :: Applicative f => Model a -> (Four a -> f b) -> f ()
forDigit_ (M m) f = traverse_ (traverse_ f) m

forRow_ :: Applicative f => Model a -> (Four (Four a) -> f b) -> f ()
forRow_ (M m) f = traverse_ f m

forColumn_ :: Applicative f => Model a -> (Four (Four a) -> f b) -> f ()
forColumn_ (M m) f = traverse_ f (sequenceA m)

forSubSq_ :: Applicative f => Model a -> (Four (Four a) -> f b) -> f ()
forSubSq_ (M m) f = traverse_ f $ fmap N $ N $ fmap sequenceA $ unN $ fmap unN m

-- | Add constraints of the initial setup.
sudokuValues :: Model (Lit s) -> Four (Four Int) -> SAT s ()
sudokuValues (M m) v = traverse_ sequenceA $ liftA2 (liftA2 f) m v
  where
    -- force the corresponding bit.
    f :: Four (Lit s) -> Int -> SAT s ()
    f (F4 l _ _ _) 1 = addClause [l]
    f (F4 _ l _ _) 2 = addClause [l]
    f (F4 _ _ l _) 3 = addClause [l]
    f (F4 _ _ _ l) 4 = addClause [l]
    f _            _ = return ()

-------------------------------------------------------------------------------
-- SAT monad
-------------------------------------------------------------------------------

newtype SAT s a = MkSAT { unSAT :: State (Int, [[Lit s]]) a }
  deriving (Functor, Applicative, Monad)

runSAT :: Functor t => (forall s. SAT s (t (Lit s))) -> (Int, [[Int]], t Int)
runSAT m = case runState (unSAT m) (1, []) of
    (a, (nVars, clauses)) -> (nVars - 1, coerce clauses, fmap coerce a)

newtype Lit s = L Int

neg :: Lit s -> Lit s
neg (L l) = L (negate l)

newLit :: SAT s (Lit s)
newLit = MkSAT $ do
    (l, cs) <- get
    put (l + 1, cs)
    return (L l)

addClause :: [Lit s] -> SAT s ()
addClause c = MkSAT $ do
    (l,cs) <- get
    put (l, c:cs)

atMostOne :: [Lit s] -> SAT s ()
atMostOne literals = mapM_ f (tails literals) where
    f :: [Lit s] -> SAT s ()
    f [] = return ()
    f (l:ls) = mapM_ (g l) ls

    g :: Lit s -> Lit s -> SAT s ()
    g l1 l2 = addClause [neg l1, neg l2]

atLeastOne :: [Lit s] -> SAT s ()
atLeastOne = addClause
