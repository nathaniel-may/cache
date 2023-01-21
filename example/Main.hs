{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Cache as C
import qualified Control.Monad.Cache.Class.MonadCache as Cache
import Control.Monad.Cache.CacheT (CacheT(..), evalCacheT)
import Control.Monad.Cache.Class.MonadCache (MonadCache)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Cache (Cache)
import Data.Hashable (Hashable)
import Data.Foldable (traverse_)


main :: IO ()
main = runProdM debugApp (C.mkCache $ Just 3)

newtype ProdM a = ProdM (CacheT String String IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadCache)

runProdM :: ProdM a -> Cache String String -> IO a
runProdM (ProdM m) = evalCacheT m

simulatedUserInput :: [String]
simulatedUserInput = ["a", "a", "b", "c", "d", "b", "c", "b", "a"]

-- in a real-world app, this could instead be a call to a database, file IO, an http request etc.
simulatedIOCall :: String -> IO String
simulatedIOCall s = pure $ case s of
    "a" -> "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    "b" -> "Curabitur scelerisque cursus placerat."
    "c" -> "Praesent ullamcorper mi at mi mattis, in vehicula mi volutpat."
    "d" -> "Aenean et eros ut justo imperdiet placerat quis at ipsum."
    _ -> ""

-- same as debugApp but does not include the explicit cache-aware printing
app :: (MonadIO m, MonadCache m, Cache.Key m ~ String, Cache.Value m ~ String) => m ()
app = traverse_ f simulatedUserInput
    where
    f x = do
        -- get the value from the cache if it's there, otherwise use the function to get it over the simulated network
        -- before adding the cache into the transformer stack, this line would look something like
        -- `resource <- liftIO . simulatedIOCall $ x`
        resource <- Cache.fetch (liftIO . simulatedIOCall) x
        -- print the resource (will look the same regardless of origin)
        liftIO $ putStrLn resource

-- executes the user input printing where the values came from, cache or the simulated network call. 
debugApp :: (MonadIO m, MonadCache m, Cache.Key m ~ String, Cache.Value m ~ String) => m ()
debugApp = do
    effects <- traverse f simulatedUserInput
    liftIO . putStrLn $ "total requests made:    " <> show (length simulatedUserInput)
    liftIO . putStrLn $ "total effects executed: " <> show (sum effects)
    where
    f x = do
        -- check if it's in the cache (not normally necessary)
        inCache <- Cache.fromCache (C.elem x)
        -- get the value from the cache if it's there, otherwise use the function to get it over the simulated network
        resource <- Cache.fetch (liftIO . simulatedIOCall) x
        -- print the resource with a marker if it is from the cache (🗃️) or a network call (🌐)
        liftIO . putStrLn $ (if inCache then "(Cache)   " else "(Network) ") <> resource
        -- count the effects
        pure $ if inCache then 0 else 1
