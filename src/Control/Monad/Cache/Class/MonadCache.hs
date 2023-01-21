{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Cache.Class.MonadCache where
    
import Prelude hiding (lookup)

import           Data.Hashable (Hashable)
import           Data.Maybe (Maybe(..))
import           Data.Cache (Cache)
import qualified Data.Cache as Cache
import           Data.Functor (($>))


class (Hashable (Key m), Monad m) => MonadCache m where
    type Key m
    type Value m
    cache :: (Cache (Key m) (Value m) -> (a, Cache (Key m) (Value m))) -> m a

insert :: MonadCache m => Key m -> Value m -> m ()
insert k v = cache (\c -> ((), Cache.insert k v c))

-- | lookup a value from the cache by its key 
lookup :: MonadCache m => Key m -> m (Maybe (Value m))
lookup k = cache (Cache.lookup k)

-- | lookup a value that depends on the entire cache
fromCache :: MonadCache m => (Cache (Key m) (Value m) -> a) -> m a
fromCache f = cache (\c -> (f c, c))

evict :: MonadCache m => Key m -> m ()
evict k = cache (\c -> ((), Cache.evict k c ))

evictAll :: MonadCache m => m ()
evictAll = cache (\c -> ((), Cache.evictAll c))

-- | gets the value using the cache if possible,
-- | otherwise it runs the effect and puts it in the cache
fetch :: MonadCache m => (Key m -> m (Value m)) -> Key m -> m (Value m)
fetch f k = lookup k >>= (\case
    Nothing -> f k >>= \v -> insert k v $> v
    Just v -> pure v)

size :: MonadCache m => m Int
size = fromCache Cache.size
