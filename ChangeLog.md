# v0.2.0

- Fix the slicing/take/drop operations

  The operations now have worse performance (linear), but are at least correct.  They will be replaced by more efficient implementations in later version.

- Implement additional fold variants (David Feuer @treeowl)
- Improve the representation of the `Vector (David Feuer @treeowl)

  It is now more compact and unboxes better. This removes some "impossible" cases from the code.

- Strictness fixes (David Feuer @treeowl)

  Includes making `snoc` more strict, which should improve performance.

- Fix the traversal order in `traverse`

  Before, the tail was traversed in the wrong order.
