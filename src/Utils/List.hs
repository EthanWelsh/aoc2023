module Utils.List (
  pairs,
  pairsByTwo,
  replace,
  pruneMatching
) where

-- | Create consecutive pairs from a list: [a,b,c] -> [(a,b),(b,c)]
-- | Overlapping consecutive pairs: [a,b,c] -> [(a,b),(b,c)]
pairs :: [a] -> [(a, a)]
pairs (a:b:xs) = (a, b) : pairs (b : xs)
pairs _ = []

-- | Non-overlapping pairs: [a,b,c,d] -> [(a,b),(c,d)]
pairsByTwo :: [a] -> [(a, a)]
pairsByTwo (a:b:xs) = (a, b) : pairsByTwo xs
pairsByTwo _ = []

-- | Replace all occurrences of an element with another element.
replace :: Eq a => a -> a -> [a] -> [a]
replace old new = map (\x -> if x == old then new else x)

-- | Filter out elements that match any predicate in the list.
-- Primary name: `pruneMatching` (keeps intent from existing codebase).
pruneMatching :: [a -> Bool] -> [a] -> [a]
pruneMatching fs = filter (not . anyMatch fs)
  where
    anyMatch :: [a -> Bool] -> a -> Bool
    anyMatch ps x = any (\p -> p x) ps
