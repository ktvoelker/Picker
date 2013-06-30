
module MaxN (MaxN(), empty, toList, insert) where

data MaxN a =
  MaxN
  { mMaxSize     :: Int
  , mSize        :: Int
  , mFull        :: Bool
  , mElems       :: [a]
  } deriving (Show)

empty :: Int -> MaxN a
empty maxSize
  | maxSize < 0 = error "MaxN.empty: maxSize < 0"
  | otherwise   = MaxN maxSize 0 (maxSize == 0) []

toList :: MaxN a -> [a]
toList = reverse . mElems

insert :: (Ord a) => a -> MaxN a -> MaxN a
insert x m@MaxN{ .. } = f $ m { mElems = insertSorted mFull x mElems }
  where
    f = case mFull of
      True  -> id
      False -> \m@MaxN{ .. } -> m { mSize = mSize + 1, mFull = mSize + 1 == mMaxSize }

insertSorted :: (Ord a) => Bool -> a -> [a] -> [a]
insertSorted _ x [] = [x]
insertSorted True x xs@(min : maxs)
  | min >= x  = xs
  | otherwise = insertSorted False x maxs
insertSorted False x xs@(min : maxs)
  | min >= x  = x : xs
  | otherwise = min : insertSorted False x maxs

