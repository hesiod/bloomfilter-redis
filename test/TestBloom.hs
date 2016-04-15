import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU hiding (assert)
import Test.QuickCheck.Monadic

import Data.List (nub)
import Data.Monoid (All(..))
import Data.Word (Word32, Word64)
import Database.Redis

import Data.RedisBloom
import Data.RedisBloom.Hash.FNV
import Data.RedisBloom.Suggestions

import Common

main :: IO ()
main = defaultMainWithIngredients [ rerunningTests [ consoleTestReporter ] ] tests

exec :: Redis a -> IO a
exec x = do
  conn <- connect defaultConnectInfo
  runRedis conn x

encode :: Num a => Bool -> a
encode False = 0
encode True = 1

valBound :: Int
valBound = 2 ^ (12 :: Int)

capacityBounds :: Gen Int
capacityBounds = choose (1, 2 ^ (16 :: Int))

elementGen :: Gen Int
elementGen = choose (negate valBound, valBound :: Int)

elementGenL :: Gen [Int]
elementGenL = listOf1 elementGen

elementGenLNubbed :: Monad m => PropertyM m [Int]
elementGenLNubbed = fmap nub . pick $ elementGenL

epsilonBounds :: Gen Double
epsilonBounds = choose (1e-6, 0.5 :: Double)

bfg :: Monad m => PropertyM m (Bloom Int, Double)
bfg = do
  cap <- pick capacityBounds
  epsilon <- pick epsilonBounds
  return (suggestCreate cap epsilon test_key, epsilon)

limit :: RealFrac a => Int -> a -> a
limit n = let k = 10 ^ n in (/k) . fromIntegral . round . (*k)

collectLabeled :: Monad m => String -> Double -> PropertyM m ()
collectLabeled name x = monitor . label $ name ++ show (limit 1 x)

tests, treeFNV, treeFixed, treeVar, treeConsistency, treeEpsilonVar, treeEpsilon :: TestTree
tests = testGroup "Tests" [treeFNV, treeConsistency, treeEpsilon]
treeFNV = testGroup "Fowler-Noll-Vo hash function" [ testGroup "FNV primes" [
           testCase "32 bits" $
            16777619 @=? (fnvPrime :: Word32),
           testCase "64 bits" $
            1099511628211 @=? (fnvPrime :: Word64)
           ]]
treeConsistency = testGroup "consistency" [treeFixed, treeVar]
treeEpsilon = testGroup "false positive rate" [treeEpsilonVar]
treeFixed = testGroup "fixed configuration"
  [ QC.testProperty "single element" $
      monadicIO $ do
        x <- pick elementGen
        b <- run . exec $ createAddQuery test_bf x
        assert b,
    QC.testProperty "multiple elements" $
      monadicIO $ do
        lx <- elementGenLNubbed
        b <- run . exec $ fmap (getAll . foldMap All) (createAddQueryL test_bf lx)
        assert b
  ]
treeVar = testGroup "variable configuration"
  [ QC.testProperty "single element" $
      monadicIO $ do
        (blt, _) <- bfg
        x <- pick elementGen
        b <- run . exec $ createAddQuery blt x
        assert b,
    QC.testProperty "multiple elements" $
      monadicIO $ do
        (blt, _) <- bfg
        len <- pick . choose $ (2^2, 2^8)
        lx <- pick $ vectorOf (min (fromIntegral $ capacity blt) len) elementGen
        let lx' = nub lx
        b <- run . exec $ fmap (getAll . foldMap All) (createAddQueryL blt lx')
        assert b
  ]
treeEpsilonVar = testGroup "variable configuration"
  [ QC.testProperty "multiple elements" $
      monadicIO $ do
        (blt, epsilon) <- bfg
        let (Capacity c) = capacity blt
            c'      = fromIntegral c :: Double
        len <-    pick . choose $ (4, min (2^8) c)
        notlen <- pick . choose $ (4, 2 * len)
        let vec     = vectorOf len elementGen
            check l = not . flip elem l
            vecn  l = vectorOf notlen . flip suchThat (check l) $ elementGen
        lx <- pick vec
        let lx' = nub lx
        lxnot <- pick $ vecn lx'
        let lxnot'  = nub lxnot
        run . exec . createAddL blt $ lx'
        falsePositives <- run . exec . queryL blt $ lxnot'
        let notlen' = fromIntegral . length $ lxnot'
            fp      = sum . fmap encode $ falsePositives
            ratio   = fp  / notlen'
            cl      = c' / fromIntegral len
        monitor . counterexample $ "Length: " ++ show len
        monitor . counterexample $ "Capacity: " ++ show c
        monitor . counterexample $ "Capacity / Length: " ++ show cl
        monitor . counterexample $ "False Positives: " ++ show fp
        monitor . counterexample $ "Actual False-Positive Ratio (α): " ++ show ratio
        monitor . counterexample $ "Expected False-Positive Ratio (ɛ): " ++ show epsilon
        collectLabeled "α = "       ratio
        collectLabeled "α ∕ ɛ = " $ ratio / epsilon
        assert $ ratio <= epsilon
  ]
