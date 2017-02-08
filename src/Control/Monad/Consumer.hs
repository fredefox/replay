{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Monad.Consumer
  ( ConsumerT
  , Consumer
  , runConsumerT
  , runConsumer
  , next
  , peek
  , skip
  , skipMaybe
  ) where

import Prelude hiding (tail)
import Data.Maybe
import Control.Monad.Identity
import Control.Monad.State

type ConsumerT s m = StateT [s] m

type Consumer s = ConsumerT s Identity

type MonadConsumer s m = MonadState [s] m

runConsumerT :: ConsumerT s m a -> [s] -> m (a, [s])
runConsumerT = runStateT

runConsumer :: Consumer s a -> [s] -> (a, [s])
runConsumer = runState

next :: MonadConsumer s m => m (Maybe s)
next = get >>= \s -> case s of
  []     -> return Nothing
  (x:xs) -> put xs >> return (Just x)

peek :: MonadConsumer s m => m (Maybe s)
peek = listToMaybe <$> get

skip :: MonadConsumer s m => m ()
skip = modify tail

skipMaybe :: MonadConsumer s m => (s -> Bool) -> m (Maybe s)
skipMaybe p = do
  x <- peek
  when (maybe False p x)
    skip
  return x

-- Needed because `Prelude.tail` is non-total
tail :: [a] -> [a]
tail []     = []
tail (_:xs) = xs
