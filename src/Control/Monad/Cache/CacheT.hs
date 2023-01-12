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


newtype CacheT k v m a = CacheT (StateT (Cache k v) m a)
deriving instance Hashable k => MonadTrans (CacheT k v)

instance (Applicative m, Hashable k) => Functor (CacheT k v m) where
    fmap f (CacheT x) = CacheT (fmap f x)

instance (Hashable k, Monad m) => Applicative (CacheT k v m) where
    pure x = CacheT $ StateT (\s -> pure (x, s))
    (CacheT (StateT mf)) <*> (CacheT (StateT mx)) = CacheT . StateT $ \ s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        pure (f x, s'')

instance (Hashable k, Monad m) => Monad (CacheT k v m) where
    (CacheT (StateT x)) >>= f = CacheT . StateT $ \s ->
        x s >>= \(v, s') -> case f v of CacheT (StateT st) -> st s'

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
