{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}

-- | Internals
module Data.RedisBloom.Internal where

import GHC.Generics
import Data.Typeable

import qualified Data.ByteString.Char8 as BS

-- | Number of hashes to use in a bloom filter.
newtype HashCount = HashCount Int deriving (Generic, Typeable, Show, Eq, Ord, Enum, Num, Real, Integral, Bounded)
-- | Capacity of a bloom filter.
newtype Capacity = Capacity Int   deriving (Generic, Typeable, Show, Eq, Ord, Enum, Num, Real, Integral)

-- | Redis Key
type Key = BS.ByteString
