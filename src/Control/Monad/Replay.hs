{- |
Module      :  Control.Monad.Replay

[Computation type:] Computations that can be replayed.

[Binding strategy:] This monad is basically a wrapper around the monad-stack:

  * ConsumerT (defined in 'Control.Monad.Consumer.ConsumerT')
  * ExceptT
  * WriterT

[Useful for:] Memoizing monadic computations.

The 'Replay' monad represents computations that may be "replayed". Replayed
computations use values from the input stream rather than performing the monadic
computation that it wraps.
-}
module Control.Monad.Replay (
  -- * The replay monad
    ReplayT
  , Replay
  -- * Construction
  , io
  , ask
  , liftR
  -- * Evaluation
  , runReplayT
  , runReplay
  , run
  , running
  , runningWith
  -- * Results
  , addAnswer
  , Trace
  , Item
  ) where

import           Data.Maybe
import           Control.Monad.Trans
import           Control.Monad.Identity
import qualified Control.Monad.Consumer as C
import           Control.Monad.Consumer (ConsumerT, runConsumerT)
import qualified Control.Monad.Except as E
import           Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Writer as W
import           Control.Monad.Writer (WriterT, runWriterT)

type ReplayT q r m a = ConsumerT (Item r) (ExceptT q (WriterT (Trace r) m)) a

runReplayT :: ReplayT q r m a -> Trace r -> m (Either q (a, Trace r), Trace r)
runReplayT r t = runWriterT . runExceptT . (`runConsumerT` t) $ r

type Replay q r a = ReplayT q r Identity a

runReplay :: Replay q r a -> Trace r -> (Either q (a, Trace r), Trace r)
runReplay r t = runIdentity . (`runReplayT` t) $ r

data Item  r = Answer r | Result String deriving (Show)
type Trace r = [Item r]

-- | Specialized version of `liftR`
io  :: (Show a, Read a) => IO a -> ReplayT q r IO a
io = liftR

tell :: Monad m => Item r -> ReplayT q r m ()
tell = W.tell . pure

-- | `ask` stops the whole program to ask the user a question, returning the
-- question along with the trace of the computation so far. The user of the
-- program can then examine the question. When the user wants to continue the
-- program they restart it with the generated trace, extended with an answer to
-- the question.
--
-- If the question has already been answered in the input trace, the answer from
-- the trace is used instead of stopping execution, and the program continues on
-- to the next ask.
ask :: Monad m => q -> ReplayT q r m r
ask q = do
  a <- askAnswer
  case a of
    Nothing -> E.throwError q
    Just a' -> tell (Answer a') >> return a'

-- | @`askFor` p@ returns the head of the stream @x@ if @p x@ is a @Just@.
askFor :: Monad m => (Item r -> Maybe x) -> ReplayT q r m (Maybe x)
askFor p = join . fmap p <$> C.skipMaybe (isJust . p)

-- | Returns the result of an action (which?) if it exists.
askResult :: Monad m => ReplayT q r m (Maybe String)
askResult = askFor result
-- askResult = result' <$> C.skipMaybe isResult
--   where
--     result' = join . fmap result

-- | Returns the answer to a question (which?) if it exists.
askAnswer :: Monad m => ReplayT q r m (Maybe r)
askAnswer = answer' <$> C.skipMaybe isAnswer
  where
    answer' = join . fmap answer

isAnswer :: Item r -> Bool
isAnswer Answer{} = True
isAnswer _        = False

result :: Item r -> Maybe String
result (Result r) = Just r
result _          = Nothing

answer :: Item r -> Maybe r
answer (Answer r) = Just r
answer _          = Nothing

-- | `run` runs a program and discards the trace if the result is a success.
-- If the result is a failure that failure and the trace is returned.
run :: Monad m => ReplayT q r m a -> Trace r -> m (Either (q, Trace r) a)
run r t = change <$> runReplayT r t
  where
    change (e, t') = case e of
      Left q       -> Left (q, t')
      Right (a, _) -> Right a

-- | @`running` r@ keeps replaying @r@ until all answers have been
-- provided. The answers are provided interactively by the user.
running :: ReplayT String String IO a -> IO a
running prog = prog `runningWith` \q -> do
    putStr ("Question: " ++ q ++ " ")
    getLine

-- | `runningWith` generalizes `running` to use an arbitrary monadic computation
-- to provide answers to questions.
runningWith :: Monad m => ReplayT q r m a -> (q -> m r) -> m a
runningWith prog oracle = play []
  where
    play t = do
      r <- run prog t
      case r of
        Left (q, t2) -> do
          a <- oracle q
          play (addAnswer t2 a)
        Right x -> return x

-- | `addAnswer` adds an answer to the end of a trace.
addAnswer :: Trace r -> r -> Trace r
addAnswer t a = t ++ pure (Answer a)

-- | `liftR` allows us to perform some monadic computation in the replay monad.
-- If there is already a result at the current point in the trace, the
-- computation is not performed and that is used instead.
liftR :: (Show a, Read a, Monad m) => m a -> ReplayT q r m a
liftR act = do
  r <- memoized
  tell (Result $ show r)
  return r
    where
      memoized = do
        r <- askResult
        case r of
          Nothing -> lift . lift . lift $ act
          Just r' -> return (read r')
