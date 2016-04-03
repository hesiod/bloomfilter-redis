{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}

-- | Suggestions based on <http://hur.st/bloomfilter>
module Data.RedisBloom.Suggestions
    (
     -- * Suggestions
     suggestCapacity, suggestHashCount,
     -- * Bloom filter creation
     suggestCreate,
     module Data.RedisBloom.Internal
    )
where

import Data.Hashable

import Data.RedisBloom
import Data.RedisBloom.Hash
import Data.RedisBloom.Internal

-- | http://hur.st/bloomfilter
-- Suggests an appropriate capacity for a given number of elements.
suggestCapacity :: forall a b. (Integral a, RealFrac b, Floating b)
                   => a -- ^ expected maximum capacity
                   -> b -- ^ desired false positive rate where 0 < /e/ < 1
                   -> Capacity
suggestCapacity n p = ceiling g
    where
      n' = fromIntegral n
      x  = n' * log p
      tw = 2 :: b
      y  = recip $ tw ** log tw
      g  = x / log y :: b

-- | Suggets an appropriate number of hash functions for a given capacity and false positive pro
suggestHashCount :: forall a. (Integral a)
                    => a -- ^ expected maximum capacity
                    -> Capacity
                    -> HashCount
suggestHashCount n m = HashCount . round $ log tw * (fromIntegral m / fromIntegral n)
    where
      tw = 2 :: Double

-- | Creates a bloom filter configuration with the specified values.
suggestCreate :: (Integral a, RealFrac b, Floating b, Hashable d)
                 => a   -- ^ expected maximum capacity
                 -> b   -- ^ desired false positive rate where 0 < /e/ < 1
                 -> Key -- ^ Redis key for the bloom filter
                 -> Bloom d
suggestCreate n p k = Bloom k ca haff
    where
      ca = suggestCapacity n p
      hc = suggestHashCount n ca
      haff = hashFamilyFNV1a hc
