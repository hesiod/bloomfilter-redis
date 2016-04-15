{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}

-- | The Fowler–Noll–Vo or FNV hash function,
-- a simple and fast hash function suitable for use in a bloom filter.
--
-- See <http://www.isthe.com/chongo/tech/comp/fnv> for
-- further information.
module Data.RedisBloom.Hash.FNV
    (
     -- * Hash functions
     fnv1, fnv1a,
     -- ** Historical
     fnv0,
     -- * Auxiliary constants
     fnvPrime, fnvOffsetBasis
    )
where

import Data.Binary (Binary, encode)
import Data.Word (Word8, Word32, Word64)
import Data.Bits (Bits(..), FiniteBits(..), shiftL, popCount)

import Math.NumberTheory.Primes.Testing (isPrime)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

{-# INLINE twoPwr #-}
twoPwr :: (Num a, Bits a, Integral bits) => bits -> a
twoPwr x = 1 `shiftL` fromIntegral x

ff :: forall a b. (Integral a, Bits a, Fractional b) => a -> b
ff 0 = 3 / 4
ff 1 = ff (0 :: Int) - recip 8
ff x = let op = if even x then (+) else (-)
           x' = fromIntegral (twoPwr (x + 2) :: a) :: b
      in ff (pred x) `op` recip x'

fd :: forall a bits. (Bits a, Integral a, Integral bits) => bits -> a
fd x = twoPwr e + twoPwr (8::Int)
    where
      flx = fromIntegral x :: Double
      x'  = max 0 . pred . round $ sqrt flx / 4 :: a
      e   = round $ flx * ff x' :: a

test :: (Bits a, Integral a) => a -> Bool
test x = x `mod` left > right
    where
      left  = twoPwr (40 :: Int) - twoPwr (24 :: Int) - 1
      right = twoPwr (24 :: Int) + twoPwr (8  :: Int) + twoPwr (7 :: Int)

findPrime :: Integral bits => bits -> Integer
findPrime s = if null primes then head candidates else head primes
    where
      bs = [ b | b <- [0..twoPwr (8 :: Int)], popCount b == 4 || popCount b == 5 ]
      candidates = filter test . fmap (\x -> fd s + x) $ bs
      primes = filter isPrime candidates

fnvPrime32 :: Word32
fnvPrime32 = $( [| fromInteger $ findPrime (32::Word32) |] )
fnvPrime64 :: Word64
fnvPrime64 = $( [| fromInteger $ findPrime (64::Word64) |] )
{-# INLINE [1] fnvPrime #-}
{-# RULES
  "prime/32" [2] fnvPrime = fnvPrime32;
  "prime/64" [2] fnvPrime = fnvPrime64;
  #-}
-- | The FNV prime. The prime is calculated
-- automatically based on the number of bits
-- in the resulting type.
-- However, primes for @2^n@ where @n@ is not
-- in the range @5..9@ are not (officialy)
-- supported.
--
-- <http://www.isthe.com/chongo/tech/comp/fnv/#FNV-param>
fnvPrime :: forall a. (Num a, FiniteBits a) => a
fnvPrime = fromInteger . findPrime . finiteBitSize $ (undefined :: a)

{-# INLINE fnvFold #-}
fnvFold :: (Num a, FiniteBits a) => Bool -> Word8 -> a -> a
fnvFold False !x !h = (fnvPrime * h) `xor` fromIntegral x
fnvFold True  !x !h = fnvPrime * (h  `xor` fromIntegral x)

-- | Variant 0 is historical and should not be used directly.
-- Rather, it is used to calculate the offset basis ('fnvOffsetBasis')
-- of the algorithm ('fnv1' and 'fnv1a').
--
-- <http://www.isthe.com/chongo/tech/comp/fnv/#FNV-0>
fnv0 :: (Binary a, Num b, FiniteBits b) => a -> b
fnv0 = B.foldr' (fnvFold False) 0 . BL.toStrict . encode

fnvOffsetBasis32 :: Word32
fnvOffsetBasis32 = $( [| fnvOffsetBasis |] )
fnvOffsetBasis64 :: Word64
fnvOffsetBasis64 = $( [| fnvOffsetBasis |] )
{-# INLINE [1] fnvOffsetBasis #-}
{-# RULES
  "offset/32" [2] fnvOffsetBasis = fnvOffsetBasis32;
  "offset/64" [2] fnvOffsetBasis = fnvOffsetBasis64;
  #-}
-- | The offset basis for the FNV hash function ('fnv1' and 'fnv1a').
--
-- <http://www.isthe.com/chongo/tech/comp/fnv/#FNV-param>
fnvOffsetBasis :: (FiniteBits a, Num a) => a
fnvOffsetBasis = fnv0 constant
    where
      constant = "chongo <Landon Curt Noll> /\\../\\" :: B.ByteString

-- These lead to infinite loops (why?)
--{-# INLINABLE fnv1  #-}
--{-# INLINABLE fnv1a #-}
fnv1, fnv1a  :: (Binary a, FiniteBits b, Num b) => a -> b
-- | Variant 1 of the FNV hash function.
-- The hash is first multiplied with the 'fnvPrime' and then 'xor'ed with the octet.
--
-- <http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1>
fnv1  = B.foldr' (fnvFold False) fnvOffsetBasis . BL.toStrict . encode
-- | Variant 1a of the FNV hash function.
-- The hash is first 'xor'ed with the octet and then multiplied with the 'fnvPrime'.
--
-- <http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1a>
fnv1a = B.foldr' (fnvFold True)  fnvOffsetBasis . BL.toStrict . encode
