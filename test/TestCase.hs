module TestCase (main) where

import Data.IORef
import Control.Monad
import Control.Monad.Replay
import System.Exit

type ReplayIO q r a = ReplayT q r IO a

emptyTrace :: [Item r]
emptyTrace = []

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  unless (and results) exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> ReplayIO () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp0 = do
    (readTick, tick) <- ticker
    x <- play (p tick) emptyTrace inp0
    n <- readTick
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | `ticker` returns is a wrapper around an IORef and exposes a way of reading
-- the tickers value and a way of incrementing (ticking) the counter.
ticker :: IO (IO Int, IO ())
ticker = do
  c <- newIORef 0
  return (readIORef c, modifyIORef c succ)

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i
  if r == r'
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of " ++ show r')
         >> return False


-- TODO: With this setup we have no way of testing what happens when `ask`'s are
-- performed for which there is no matching result in the input. We essentially
-- have no way of expecting `Left`'s as a result from `run`.
--
-- TODO: We also do not have a way of testing the memoization mechanism since
-- all inputs in this test-module are appended to the input stream via
-- `addAnswer`.
--
-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    }
  , TestCase
    { testName    = "tick twice"
    , testInput   = [4]
    , testResult  = (0, 2)
    , testProgram = \tick -> io tick >> io tick >> return 0
    }
  , TestCase
    { testName    = "return"
    , testInput   = []
    , testResult  = (0, 0)
    , testProgram = const (return 0)
    }
  ]

-- | Running all the test cases.
runTests = mapM checkTestCase testCases

