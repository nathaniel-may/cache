module Data.Cache where

import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.Vector (Vector)
import qualified Data.Vector as V


-- | a data type for caching a fixed or unlimited number of elements.
-- | for fixed caches, when the cache overflows it will evict the least-recently touched element.
data Cache k v = Cache (Maybe Int) (Vector k) (HashMap k v)

-- | constuctor for an empty cache with the designated maximum elements.
-- | a limit of `Nothing` will create an unlimited cache.
mkCache :: Hashable k => Maybe Int -> Cache k v
mkCache max = Cache max V.empty M.empty

-- | create a cache from an existing HashMap. If the map is bigger than the proided
-- | cache size, elements will be evicted till it reaches the correct size. No order to
-- | these evictions is guaranteed.
fromHashMap :: Hashable k => Maybe Int -> HashMap k v -> Cache k v
fromHashMap max m = shrink (Cache max (V.fromList $ M.keys m) m)

insert :: Hashable k => k -> v -> Cache k v -> Cache k v
insert k v (Cache max ks m) =
    let m' = M.insert k v m
        ks' = touch k ks
    in shrink (Cache max ks' m')

-- | gets an element from the Cache if it does not exist or update its position to
-- | the most recently touched. Process the effects of the last element if the maximum size is exceeded
lookup :: Hashable k => k -> Cache k v -> (Maybe v, Cache k v)
lookup k (Cache max ks m) =
    let v = M.lookup k m
        ks' = if isJust v then touch k ks else ks
    in (v, shrink $ Cache max ks' m)

-- | immediately removes the element from the cache
evict :: Hashable k => k -> Cache k v -> Cache k v
evict k (Cache max ks m) = Cache max (V.filter (/= k) ks) (M.delete k m)

-- | immediately removes all elements from the cache including pinned elements
evictAll :: Hashable k => Cache k v -> Cache k v
evictAll (Cache max _ _) = Cache max V.empty M.empty

-- | remove the least-recently touched element from the cache
pop :: Hashable k => Cache k v -> Cache k v
pop (Cache max ks m) = fromMaybe (Cache max V.empty m) $ do
    (ks', k) <- V.unsnoc ks
    let m' = M.delete k m
    pure $ Cache max ks' m'

-- | remove the least-recently touched elements until the cache is within its maximum size
shrink :: Hashable k => Cache k v -> Cache k v
shrink input@(Cache Nothing _ _) = input
shrink input@(Cache (Just max) ks _)
    | V.length ks <= max = input
    | otherwise = shrink (pop input)

-- | similar to insert, however it this also pins an element to the cache so it will not be evicted by the rolling effect.
-- | Fails if the cache is at its max size with all pinned values already.
pin :: Hashable k => k -> v -> Cache k v -> Maybe (Cache k v)
pin k v (Cache Nothing ks m) =
    Just $ Cache Nothing (V.filter (/= k) ks) (M.insert k v m)
pin k v (Cache (Just max) ks m) = 
    if V.null ks && M.size m >= max
    then Nothing
    else Just $ Cache (Just max) (V.filter (/= k) ks) (M.insert k v m)

-- | unpins an element, but does not evict it from the cache. after unpinning,
-- | it will be the most-recently touched element in the cache. If the key was
-- | never pinned, the same cache is returned. 
unpin :: Hashable k => k -> Cache k v -> Cache k v
unpin k input@(Cache max ks m) =
    if M.member k m
    then Cache (fmap (+ 1) max) (V.cons k ks) m
    else input

size :: Hashable k => Cache k v -> Int
size (Cache _ _ m) = M.size m

-- TODO move to internal module
-- | moves the element to the front of the list if it exists, or prepends it otherwise.
-- | Used to update the order of cache eviction.
touch :: Eq a => a -> Vector a -> Vector a
touch x = V.cons x . V.filter (/= x)
