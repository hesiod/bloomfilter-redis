{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | Hash function family suitable for use in a bloom filter.
module Data.RedisBloom.Hash.Families
    (
     -- * Types
     Index,
     -- ** Hash families
     Hash, HashFamily, HashFunction,
     -- ** Raw hash functions
     RawHash, RawHashFunction,
     -- * Creating hash families
     makeIndexedHash, makeHashFamily
    )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Bits (shiftR, finiteBitSize, (.&.))
import Data.Word (Word32, Word64)
import Data.Hashable (Hashable)

import Data.RedisBloom.Internal

-- | A single hash.
type Hash = Word32
-- | An index into the hash function family.
type Index = Int
-- | A family of hashes.
type HashFamily a   = (a -> [Hash])
-- | A single indexed hash function, part of a family.
type HashFunction a = (Index -> a -> Hash)

-- | A raw hash.
type RawHash = Word64
-- | A raw hash function.
type RawHashFunction a = (a -> RawHash)

-- | Makes a indexed hash function out of a single 64-bit hash
-- function where 'i' is
-- an index in the range @0..30@ indicating
-- the member function to be computed.
--
-- Just like the original bloom filter package, a variant of
-- Kirsch and Mitzenmacher's technique from \"Less
-- Hashing, Same Performance: Building a Better Bloom Filter\",
-- <https://www.eecs.harvard.edu/~michaelm/postscripts/tr-02-05.pdf> is used here.
--
-- Quoting from the non-Redis bloomfilter package:
-- "Where Kirsch and Mitzenmacher multiply the second hash by a
-- coefficient, we shift right by the coefficient.  This offers better
-- performance (as a shift is much cheaper than a multiply), and the
-- low order bits of the final hash stay well mixed."
makeIndexedHash :: Hashable a => RawHashFunction a -> HashFunction a
makeIndexedHash hh i x = h1 + (h2 `shiftR` i)
    where
      bs = finiteBitSize (undefined :: Word32)
      h  = hh x :: Word64
      mb = fromIntegral (maxBound :: Word32) - 1 :: Word64
      h1 = fromIntegral $  h              .&. mb :: Word32
      h2 = fromIntegral $ (h `shiftR` bs) .&. mb :: Word32

-- | Makes a hash function family from a raw hash function.
makeHashFamily :: Hashable a => RawHashFunction a -> HashCount -> HashFamily a
makeHashFamily raw (HashCount n) x = uncurry ih <$> zip [1..n] xs
    where
      ih = makeIndexedHash raw
      xs = replicate (fromIntegral n) x
