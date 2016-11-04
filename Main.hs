{-# LANGUAGE TypeApplications, FlexibleContexts #-}

module Main where

import Example

import DirectUni (directUni1, directUni2)
import AutoUni (autoUni1, autoUni2)

import Generate

import Criterion.Main

-- TODO: try specialize pragma
-- TODO: try with ifTrue

our1 d = apply_ @(MonoMatch D) (monoApp testFun1) (generateA d)
our2 d = apply_ @(MonoMatch C) (monoApp testFun2) (generateA d)
cond1 d = applySelective_ @(MonoMatch D) (monoApp testFun1) (const True) (generateA d)
cond2 d = applySelective_ @(MonoMatch C) (monoApp testFun2) (const True) (generateA d)

-- uniplate d = 

example = generateA 21


main = defaultMain
  [ bgroup "generation" 
    [ bench "61" $ nf generateA 61
    , bench "101" $ nf generateA 101
    , bench "141" $ nf generateA 141
    ]
  , bgroup "our"
    [ bgroup "deep" 
        [ bench "61" $ nf our1 61
        , bench "101" $ nf our1 101
        , bench "141" $ nf our1 141]
    , bgroup "shallow" 
        [ bench "61" $ nf our2 61
        , bench "101" $ nf our2 101
        , bench "141" $ nf our2 141]
    ]
  , bgroup "conditional"
    [ bgroup "deep" 
        [ bench "61" $ nf cond1 61
        , bench "101" $ nf cond1 101
        , bench "141" $ nf cond1 141]
    , bgroup "shallow" 
        [ bench "61" $ nf cond2 61
        , bench "101" $ nf cond2 101
        , bench "141" $ nf cond2 141]
    ]
  , bgroup "autoUni" 
    [ bgroup "deep" 
        [ bench "61" $ nf autoUni1 61
        , bench "101" $ nf autoUni1 101
        , bench "141" $ nf autoUni1 141]
    , bgroup "shallow" 
        [ bench "61" $ nf autoUni2 61
        , bench "101" $ nf autoUni2 101
        , bench "141" $ nf autoUni2 141]
    ]
  , bgroup "directUni" 
    [ bgroup "deep" 
        [ bench "61" $ nf directUni1 61
        , bench "101" $ nf directUni1 101
        , bench "141" $ nf directUni1 141]
    , bgroup "shallow" 
        [ bench "61" $ nf directUni2 61
        , bench "101" $ nf directUni2 101
        , bench "141" $ nf directUni2 141]
    ]
  ]