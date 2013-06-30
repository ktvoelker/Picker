
TODO
====

Enumerate all non-ignored files in the directory heirarchy.

For each file, start with the basename and then go up through each parent directory
name to the root. Construct a [Integer] where each Integer corresponds to the number
of tokens that matched the name component at that position. (Note that when a token
matches, it is excluded from matching higher up in the tree.) (The Integers should
actually all be negative, so that the resulting lists can be directly used as sort
keys.)

  Note that all matches should be case-insensitive.

