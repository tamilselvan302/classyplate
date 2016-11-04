{-# LANGUAGE TypeApplications #-}

module Main where

import Example

import DirectUni (directUni)
import AutoUni (autoUni)

import Generate

import Criterion.Main

our d = apply @(MonoMatch D) (monoApp testFun) (generateA d)

cond d = applySelective @(MonoMatch D) (monoApp testFun) (const True) (generateA d)

-- our d = apply @(MonoMatch C) (monoApp testFun) (generateA d)

-- uniplate d = 

example = generateA 21


main = defaultMain
  [ bgroup "generation" 
    [ bench "61" $ nf generateA 61
    , bench "101" $ nf generateA 101
    , bench "141" $ nf generateA 141
    ]
  , bgroup "our"
    [ bench "61" $ nf our 61
    , bench "101" $ nf our 101
    , bench "141" $ nf our 141
    ]
  , bgroup "conditional"
    [ bench "61" $ nf cond 61
    , bench "101" $ nf cond 101
    , bench "141" $ nf cond 141
    ]
  , bgroup "autoUni" 
    [ bench "61" $ nf autoUni 61
    , bench "101" $ nf autoUni 101
    , bench "141" $ nf autoUni 141
    ]
  , bgroup "directUni" 
    [ bench "61" $ nf directUni 61
    , bench "101" $ nf directUni 101
    , bench "141" $ nf directUni 141
    ]
  ]