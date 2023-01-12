{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Cache.CacheT where

import Control.Monad.Cache.Class.MonadCache (MonadCache, Key, Value, cache)
import Control.Monad.State (MonadTrans, StateT(..), evalStateT, execStateT, lift, runStateT)
import Data.Hashable (Hashable)
import Data.Cache (Cache)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)


-- | monad transformer for storing key-value pairs into a fixed-size, automatically evicted cache.
newtype CacheT k v m a = CacheT (StateT (Cache k v) m a)

deriving instance (Functor m, Hashable k) => Functor (CacheT k v m)
deriving instance (Hashable k, Monad m) => Applicative (CacheT k v m) 
deriving instance (Hashable k, Monad m) => Monad (CacheT k v m)
deriving instance Hashable k => MonadTrans (CacheT k v)

instance (Hashable k, Monad m) => MonadCache (CacheT k v m) where
    type instance Key (CacheT k v m) = k
    type instance Value (CacheT k v m) = v
    cache f = CacheT (StateT $ pure . f)

instance (Hashable k, MonadIO m) => MonadIO (CacheT k v m) where
    liftIO = lift . liftIO

runCacheT :: Monad m => CacheT k v m a -> Cache k v -> m (a, Cache k v)
runCacheT (CacheT x) = runStateT x

evalCacheT :: Monad m => CacheT k v m a -> Cache k v -> m a
evalCacheT (CacheT x) = evalStateT x

execCacheT :: Monad m => CacheT k v m a -> Cache k v -> m (Cache k v)
execCacheT (CacheT x) = execStateT x
