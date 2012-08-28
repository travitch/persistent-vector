# Persistent Vector

A library providing persistent (purely functional) vectors for Haskell
based on array mapped tries.

## Description

These persistent vectors are modeled on the persistent vector used by
clojure, with an API modeled after Data.Sequence from the containers
library.  This data structure is *spine strict* and is not useful for
incremental consumption.  If you need that, stick to lists.  It is
still lazy in the elements.

While per-element operations are O(log(n)), the internal tree can
never be more than 7 or 8 deep.  Thus, they are effectively constant
time.

This implementation adds O(1) slicing support for vectors that I do
not believe clojure supports.  The implementation cheats, though, and
slices can retain references to objects that cannot be indexed.

## Performance

Performance is an important consideration for a data structure like
this.  The package contains a criterion benchmark suite that attempts
to compare the performance of persistent vectors against a variety of
existing persistent data structures.  As an overview of the results I
have observed:

 * Traversing and building lists is faster than the same operations
   with persistent vectors.

 * (Strict) left folds over persistent vectors are faster than left
   folds over Sequences.  Right folds over Sequences are faster than
   right folds over vectors.

 * Indexing persistent vectors is faster than indexing sequences and
   IntMaps (and, of course, lists).

 * Appending to vectors is slightly faster than appending to a Sequence.
   It is much faster than appending to an IntMap.

 * Updating an element at an index in a vector is *slower* than
   updating an index in a Sequence (but still faster than an IntMap).

Overall, it seems like persistent vectors are efficient at most tasks.
If you only need a (strict) left fold, they are efficient for
traversal.  Indexing and construction are very fast, but Sequences are
superior for element-wise updates.

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
