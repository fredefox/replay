module Control.Monad.Replay
  ( io
  , ask
  , run
  ) where

import Control.Monad.IO.Class

import Data.Time

  -- Maybe we could parameterize the IO.
newtype Replay q r a = Replay
  { runReplay :: Trace r -> IO (Either (q, Trace r) a)
  }

instance Functor (Replay q r) where
  fmap f (Replay rpl) = Replay $ \t -> fmap f <$> rpl t

instance Applicative (Replay q r) where
  pure = Replay . pure . pure . pure
  -- Which one is better here?:
  mf <*> ma = mf >>= (`fmap` ma)
  -- (Replay mf) <*> (Replay ma) = Replay $ \t -> do
  --   k <- mf t
  --   case k of
  --     Left (q, t') -> return $ Left (q, t')
  --     Right f -> fmap f <$> ma t

instance Monad (Replay q r) where
  return = pure
  (Replay ma) >>= f = Replay $ \t -> do
    a <- ma t
    either (return . Left) ((`runReplay` t) . f) a

instance MonadIO (Replay q r) where
--  liftIO act = Replay $ \_ -> fmap Right act
--  liftIO :: IO a -> Replay q r a
  liftIO act = Replay $ \_trace -> do
    a <- act
    return (Right a)

-- `io` wierdly puts the result of the IO-action into the trace -
-- hence the Show requirement.
-- Please note that I've changed the question-type thereby
-- diverging from the spec.
io :: (Show a, Read a) => IO a -> Replay q r a
--io act = Replay $ \t -> act >>= \a -> undefined
io = liftIO

-- io act = do
--   a <- liftIO act
--   modifyTrace (`addResult` a)

-- --modifyTrace :: (Show a, Read q) => (Trace r -> Trace r) -> Replay q r a
-- modifyTrace :: (Trace r -> Trace r) -> Replay q r a
-- modifyTrace f = Replay $ \t -> return (Left (_, f t))

ask :: q -> Replay q r a
ask q = Replay $ \t -> return . Left $ (q, t)

-- Could we use:
--     Serialize r => Replay q r a -> Trace r -> IO (Either (q, t), a)
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = runReplay

type Trace a = [Item a]

type Item a = Either a String

addAnswer :: Trace a -> a -> Trace a
addAnswer t r = t ++ pure (Left r)

addResult :: Show a => Trace r -> a -> Trace r
addResult t r = t ++ pure (Right $ show r)

example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)
