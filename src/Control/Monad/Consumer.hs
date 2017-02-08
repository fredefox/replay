{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{- |
Module : Control.Monad.Consumer

Consumer is a simple wrapper around the state monad. The state in a consumer is
a stream (implemented with lists). A consumer can remove values (consume) from
the head of the list.
-}
module Control.Monad.Consumer (
  -- * The consumer monad
    ConsumerT
  , Consumer
  -- * Evaluation
  , runConsumerT
  , runConsumer
  -- * Construction
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

-- | Consumes the next input from the stream if it is non-empty.
next :: MonadConsumer s m => m (Maybe s)
next = do
  r <- peek
  skip
  return r

-- | Returns the head of the stream without consuming it.
peek :: MonadConsumer s m => m (Maybe s)
peek = listToMaybe <$> get

-- | Consumes the head of the stream and ignores the result.
skip :: MonadConsumer s m => m ()
skip = modify tail

-- | Consumes the head of the stream iff `p x` holds where x is the head of the
-- stream.
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
