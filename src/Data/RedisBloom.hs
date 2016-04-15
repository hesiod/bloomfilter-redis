{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

-- | A bloom filter for the Redis in-memory store.
module Data.RedisBloom
    (
     -- * Bloom filter configuration
     -- ** Fundamental types
     module Data.RedisBloom.Internal,
     -- ** Static bloom filter configuration
     Bloom(..),
     -- * Bloom filter operations
     createBF, createIfNewBF, addBF, queryBF
    ) where

import Data.Monoid (All(..))

import Data.ByteString.Char8 (pack)
import Database.Redis

import Data.RedisBloom.Hash
import Data.RedisBloom.Internal

-- | Bloom filter static configuration.
-- To use suggested values based on the desired
-- false-positive rate and capacity, use 'Data.RedisBloom.Suggestions.suggestCreate'.
data Bloom a = Bloom {
      -- | The key to store the bloom filter under.
      key :: !Key,
      -- | Bloom filter capacity, i.e. the number of bits used.
      capacity :: !Capacity,
      -- | The hash family associated with the bloom filter.
      -- See 'Data.RedisBloom.Hash.hashFamilyFNV' and 'Data.RedisBloom.Hash.hashFamilySimple'
      hf :: HashFamily a
    }

-- | Create a new bloom filter with the specified configuration.
createBF :: (RedisCtx m (Either Reply)) => Bloom a -> m (Either Reply Status)
createBF bf = set (key bf) empty
    where
      empty = pack ""
-- | Create a new bloom filter with the specified configuration if the specified key does not yet exist.
createIfNewBF :: (RedisCtx m (Either Reply)) => Bloom a -> m (Either Reply Bool)
createIfNewBF bf = setnx (key bf) empty
    where
      empty = pack ""

-- | Add an element to an existing bloom filter.
addBF :: (RedisCtx m f) => Bloom a -> a -> m ()
addBF bf = mapM_ (flip (setbit (key bf)) one) . fmap (toInteger . (`mod` cap) . fromIntegral) . hf bf
    where
      (Capacity cap) = capacity bf
      one = pack "1"

getBit :: (MonadRedis m, RedisCtx m (Either Reply)) => Bloom a -> Integer -> m Bool
getBit bf i = do
  r <- getbit (key bf) i
  let l = case r of
             Left _ -> False
             Right j -> j >= 1
  return l

-- | Query whether an element exists in the bloom filter.
--
-- Gracefully fails upon failure by returning 'False'.
queryBF :: (MonadRedis m, RedisCtx m (Either Reply)) => Bloom a -> a -> m Bool
queryBF bf = query (capacity bf) (getBit bf) (hf bf)

query :: Monad m => Capacity -> (Integer -> m Bool) -> HashFamily a -> a -> m Bool
query (Capacity c) q hashf x = do
  let hashes = fmap (toInteger . (`mod` c) . fromIntegral) . hashf $ x
  lookupMany q hashes

lookupMany :: (Traversable t, Monad m) => (a -> m Bool) -> t a -> m Bool
lookupMany lookupBit hashes = do
  bools <- mapM lookupBit hashes
  return . getAll . foldMap All $ bools
