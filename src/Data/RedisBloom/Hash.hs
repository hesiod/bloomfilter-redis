{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}

-- | Hash function families suitable for use in a bloom filter.
module Data.RedisBloom.Hash
    (
     -- * Hash families
     hashFamilySimple, hashFamilyFNV1, hashFamilyFNV1a,
     module Data.RedisBloom.Hash.Families
    )
where

import Data.Hashable

import Data.RedisBloom.Hash.Families
import Data.RedisBloom.Hash.FNV
import Data.RedisBloom.Internal

makeFromHashable :: Hashable a => RawHashFunction Int -> HashCount -> HashFamily a
makeFromHashable f = makeHashFamily $ f . hashWithSalt salt
    where
      salt = 5534023222112865484

hashFamilySimple, hashFamilyFNV1, hashFamilyFNV1a :: Hashable a => HashCount -> HashFamily a
-- | A simple hash function family.
hashFamilySimple = makeFromHashable fromIntegral
-- | A hash function family based on the Fowler–Noll–Vo hash function, Variant 1.
-- See <http://www.isthe.com/chongo/tech/comp/fnv/>
hashFamilyFNV1  = makeFromHashable fnv1
-- | A hash function family based on the Fowler–Noll–Vo hash function, Variant 1a.
-- See <http://www.isthe.com/chongo/tech/comp/fnv/>
hashFamilyFNV1a = makeFromHashable fnv1a
