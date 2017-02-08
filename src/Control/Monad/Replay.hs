module Control.Monad.Replay
  ( io
  , ask
  , run
  , running
  ) where

import           Data.Maybe
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import qualified Control.Monad.Consumer as C
import           Control.Monad.Consumer (ConsumerT, runConsumerT)
import qualified Control.Monad.Except as E
import           Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Writer as W
import           Control.Monad.Writer (WriterT, runWriterT)

import Data.Time
import Debug.Trace

type ReplayT q r m a = ConsumerT (Item r) (ExceptT q (WriterT (Trace r) m)) a

runReplayT :: ReplayT q r m a -> Trace r -> m (Either q (a, Trace r), Trace r)
runReplayT r t = runWriterT . runExceptT . (`runConsumerT` t) $ r

data Item  r = Answer r | Result String deriving (Show)
type Trace r = [Item r]

-- | `io` allows us to perform IO actions in our computation. If there is
-- already a result for an io computation in the input trace, the computation is
-- not performed and the result from the trace is used instead.
io  :: (Show a, Read a) => IO a -> ReplayT q r IO a
io act = do
  r <- memoized
  tell (Result $ show r)
  return r
    where
      memoized = do
        r <- askResult
        case r of
          Nothing -> liftIO act
          Just r' -> return (read r')

tell :: Monad m => Item r -> ReplayT q r m ()
tell = W.tell . pure

-- | ask stops the whole program to ask the user a question, returning the
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

-- | `askFor p` returns the head of the stream `x` if `p x` is a `Just`.
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

-- | `running r` keeps replaying `r` until all answers have been
-- provided. The answers are provided interactively by the user.
running :: ReplayT String String IO a -> IO a
running prog = play []
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q,t2) -> do
        putStr ("Question: " ++ q ++ " ")
        a <- getLine
        play (addAnswer t2 a)
      Right x -> return x

-- | `addAnswer` adds an answer to the end of a trace.
addAnswer :: [Item r] -> r -> [Item r]
addAnswer t a = t ++ pure (Answer a)
