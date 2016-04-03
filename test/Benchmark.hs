import Criterion
import Criterion.Main
import Criterion.Types
import System.Random

import Data.RedisBloom.Hash
import Data.RedisBloom.Suggestions

setup :: a -> IO (a, Int, [Int], [Int])
setup z = do
  gen <- getStdGen
  let rr   = randoms gen
      few  = take 64               rr
      many = take 4096 . drop 64 $ rr
      one  = head                  rr
  return $ z `seq` (z, one, few, many)

config :: Config
config = defaultConfig { reportFile = Just "report.html" }

esetup :: (HashCount -> Int -> [Hash]) -> String -> Benchmark
esetup f name = env (setup . f . HashCount $ 16) $ \ ~(g, x, few, many) -> bgroup name [
       bench "single" $ nf g x,
       bench "multiple-64" $ nf (fmap g) few,
       bench "multiple-4096" $ nf (fmap g) many
  ]

main :: IO ()
main = defaultMainWith config [
        esetup hashFamilyFNV1  "FNV-1",
        esetup hashFamilyFNV1a "FNV-1a"]
