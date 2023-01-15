{-# LANGUAGE TemplateHaskell #-}

module CacheT.Prop (tests) where

import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (quickCheckAll)
import Data.Cache (Cache(..), mkCache)
import qualified Data.Cache as Cache


prop_insert_never_overflows_cache :: [Int] -> Bool
prop_insert_never_overflows_cache ks = 
    let cache = foldr (\k -> Cache.insert k k) (mkCache $ Just 3) ks
    in Cache.size cache <= 3

return []
tests :: IO b
tests = $quickCheckAll >>= \x -> if x then exitSuccess else exitFailure
