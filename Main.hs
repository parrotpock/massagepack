module Main where

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Criterion.Main

import Data.Maybe
import Data.Int (Int32)
import System.CPUTime
import Text.Printf
import qualified Data.ByteString.Lazy as BS
import MsgPack as MP
import Control.Monad (replicateM)

benchmarkFile fileName = do
  contents <- BS.readFile fileName
  return $ (MP.unpack contents :: Maybe [Int32])

benchmark val = MP.unpack $ MP.pack val

main = defaultMain [ -- TODO: Add a benchmark to write this file out first -- bgroup "fileIO" [bench "foo.dat" $ nfIO (benchmarkFile "foo.dat")]
                    bgroup "[Int32]" [bench "List of 20000 elems" $ nf (benchmark :: [Int32] -> Maybe [Int32]) [1..20000]]
                  , bgroup "[Int32]" [bench "List of 2000 elems" $ nf (benchmark :: [Int32] -> Maybe [Int32]) [1..2000]]
                  , bgroup "Int32" [bench "1" $ nf (benchmark :: Int32 -> Maybe Int32) 1, bench "2" $ whnf (benchmark :: Int32 -> Maybe Int32) 2]
                  , bgroup "Bool" [bench "False" $ nf (benchmark :: Bool -> Maybe Bool) False, bench "True" $ whnf (benchmark :: Bool -> Maybe Bool) True]
                   ]
