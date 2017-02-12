module Main where

import Data.Either
import Control.Exception
import Control.Monad
import System.Exit

import qualified TestCase
import qualified TestProp

main :: IO ()
main = do
  e0 <- tryHard TestCase.main
  e1 <- tryHard TestProp.main
  when (isLeft (e0 >> e1)) exitFailure
  exitSuccess

tryHard :: IO a -> IO (Either SomeException a)
tryHard = try
