# Persistent Vector

A library providing persistent (purely functional) vectors for Haskell
based on array mapped tries.

## Description

These persistent vectors are modeled on the persistent vector used by
clojure, with an API modeled after Data.Sequence from the containers
library.

While per-element operations are O(log(n)), the internal tree can
never be more than 7 or 8 deep.  Thus, they are effectively constant
time.

This implementation adds O(1) slicing support for vectors that I do
not believe clojure supports.  The implementation cheats, though, and
slices can retain references to objects that cannot be indexed.

## Implementation

## TODO

 * More of the Data.Sequence API

 * More efficient Eq and Ord instances.  This is tricky in the
   presence of slicing.  There are faster implementations for unsliced
   inputs.

 * Implement something to make parallel reductions simple (maybe
   something like vector-strategies)

 * Implement cons.  Cons can use the space that is hidden by the
   offset cheaply.  It can also make a variant of pushTail
   (pushHead) that allocates fragments of preceeding sub-trees.
   Each cons call will modify the offset of its result vector.
