module Test.Fluent (shouldBe) where

import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property             as Property

shouldBe :: (Show a, Eq a) => a -> a -> Property.Result
shouldBe actual expected = MkResult
                             (Just (expected == actual))
                             True
                             (unlines ["Expected: " ++ show expected, "Actual: " ++ show actual])
                             Nothing
                             False
                             Map.empty
                             Set.empty
                             []
