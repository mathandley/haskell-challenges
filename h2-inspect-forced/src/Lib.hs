{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib ( Tree (..), materializeForcedBy ) where

import Control.Exception ( evaluate )
import Control.Monad     ( void )
import Data.IORef        ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe  ( unsafeInterleaveIO, unsafePerformIO )

data Tree
  = Leaf
  | Fork Tree Int Tree
  deriving (Show, Eq)

-- Probably need some no-inlines, but passes the tests anyway
materializeForcedBy :: (Tree -> Int) -> Tree -> Tree
materializeForcedBy f t
  = unsafePerformIO $ do
      ref <- newIORef False
      t'  <- unsafeInterleaveIO (logEvalTree ref t)
      void (evaluate (f t'))
      checkEvalTree ref t'
  where
    -- When @a@ is forced, @True@ will be written to the ref
    logEval :: IORef Bool -> a -> IO a
    logEval ref x = writeIORef ref True >> pure x

    -- Interleaving is necessary to stop preemptive forcing
    logEvalTree :: IORef Bool -> Tree -> IO Tree
    logEvalTree ref t = case t of
      Leaf -> logEval ref Leaf
      Fork l x r -> logEval ref =<<
        (Fork <$> unsafeInterleaveIO (logEvalTree ref l)
              <*> unsafeInterleaveIO (logEval ref x)
              <*> unsafeInterleaveIO (logEvalTree ref r)
        )

    checkEval :: IORef Bool -> a -> IO Bool
    checkEval ref x = do
      writeIORef ref False
      -- Check if *this* evaluation causes the thunk to be evaluated
      -- If it's already in WHNF then the ref will stay @False@
      void (evaluate x)
      readIORef ref

    -- No need to interleave here as @f@ has already been applied
    checkEvalTree :: IORef Bool -> Tree -> IO Tree
    checkEvalTree ref t = do
      b <- checkEval ref t
      if b then pure Leaf
           else case t of
             Leaf -> pure Leaf
             Fork l x r ->
               Fork <$> checkEvalTree ref l
                    <*> (do b <- checkEval ref x
                            pure (if b then 0 else x)
                        )
                    <*> checkEvalTree ref r