{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module : PropTest

Property based checking of Replay
-}
module TestProp (main) where

import Data.Functor.Identity
import Data.Void

import Control.Monad.Replay

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All
import Test.QuickCheck.Property (Property(..))

main :: IO ()
main = testAll

testAll :: IO ()
testAll = testAsk >> testLift

propAsk :: (Eq a, Eq b, Show a, Show b) => a  -> [Item b] -> Property
propAsk a xs = runReplay (ask a) xs === case xs of
  (Answer r:xss) -> (Right (r, xss), [Answer r])
  _  -> (Left a, [])

testAsk :: IO ()
testAsk = quickCheck (propAsk :: Bool -> [Item Bool] -> Property)

propLift :: forall a b . (Eq a, Eq b, Show a, Show b, Read a) => Identity a -> [Item b] -> Property
propLift m xs = runReplay (liftR m) xs === case xs of
  -- if the head is a result that parses to an `a` -- if it's an unsuccesfull
  -- parse, then it's a runtime-error.
  (Result s:xss) -> (Right (read s, xss) :: Either Void (a, Trace b), [Result s])
  _              -> (Right (a, xs), [Result (show a)]) where a = runIdentity m

-- Please note that this test-case only works because we're only instantiating
-- the `a` and `b` in `propLift` with bool. And we have made `Item a` an
-- instance of `Arbitrary` in such a way that `Bool`'s are the only possible
-- results.
testLift :: IO ()
testLift = quickCheck (propLift :: Identity Bool -> Trace Bool -> Property)

instance Arbitrary a => Arbitrary (Item a) where
  arbitrary = oneof [Answer <$> arbitrary, Result . show <$> bools]
    where
      bools :: Gen Bool
      bools = arbitrary

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
