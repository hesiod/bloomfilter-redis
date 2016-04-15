# bloomfilter-redis [![Build Status](https://travis-ci.org/hesiod/bloomfilter-redis.svg?branch=master)](https://travis-ci.org/hesiod/bloomfilter-redis)

Distributed bloom filters on Redis (using the Hedis client).

The hash family algorithm is partly inspired by
[Brian O'Sullivan's bloomfilter package](https://hackage.haskell.org/package/bloomfilter).

## Features

* Implementation of the FNV-1/FNV-1a hash function is included
* Automatic derivation of a hash family from a single hash function
  as described by Kirsch and Mitzenmacher
* The bloom filter is distributed without extra effort since
  Redis does the heavy lifting
* Every `Hashable` type can be added to the bloom filter
* Every `Binary` type can be hashed

## Benchmark and Testing suite

A benchmark for the FNV hash function is included and can
be invoked using `cabal bench` or `stack bench`.
An HTML report is generated as `report.html`.

A testing suite using `tasty` is included.

## Further Information
### Todo

* Separate the FNV hash function into a separate package
* The actual operations (`addBF`, `queryBF`, etc) should
  ideally live in a `MonadReader (Bloom a)`, but this requires
  some work on the Hedis side because of `RedisCtx`

### Caveats

* The only supported FNV hash sizes are 32 and 64 bits.
  Support for larger widths is a matter of having a
  data type with instances for `FiniteBits` and `Num`.
* The offset basis (`fnvOffsetBasis`) is not correctly
  computed, although this has absolutely no effect on
  the performance of the hash function in practice.
