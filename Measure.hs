{-# LANGUAGE TypeApplications #-}

module Measure where

import Example
import Criterion.Main

generateA :: Int -> A
generateA d = ABC (generateB (d - 1)) (generateC (d - 1))

generateB :: Int -> B
generateB d 
  | d <= 0    = B 
  | otherwise = BA (generateA (d - 1))

generateC :: Int -> C
generateC d 
  | d `mod` 2 == 0 = CB (generateB (d - 1))
  | otherwise      = CD (generateD (d - 1))

generateD :: Int -> D
generateD d 
  | d <= 0    = D 
  | otherwise = DDE (generateD (d - 1)) E


testFun (CB b) = CB (BA (ABC b (CB B)))
testFun c = c

our d = apply @(MonoMatch C) (monoApp testFun) (generateA d)

sizeA (ABC b c) = 1 + sizeB b + sizeC c
sizeB B = 1
sizeB (BA a) = 1 + sizeA a
sizeC (CB b) = 1 + sizeB b
sizeC (CD d) = 1 + sizeD d
sizeD D = 1
sizeD (DDE d e) = 1 + sizeD d + 1

-- uniplate d = 

example = generateA 21


main = defaultMain [
  bgroup "our" [ bench "61" $ nf our 61
               , bench "101" $ nf our 101
               , bench "141" $ nf our 141
               ]
  ]