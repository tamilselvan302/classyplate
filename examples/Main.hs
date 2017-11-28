{-# LANGUAGE TypeApplications, FlexibleContexts, RankNTypes, DataKinds, LambdaCase, ScopedTypeVariables, ConstraintKinds, AllowAmbiguousTypes #-}

module Main where


import Data.Generics.ClassyPlate
import Example

import DirectUni (directUni1, directUni2)
import AutoUni (autoUni1, autoUni2)

import Generate

import Criterion.Main

our1 d = bottomUp @(MonoMatch D) (monoApp testFun1) (generateA d)
our2 d = bottomUp @(MonoMatch C) (monoApp testFun2) (generateA d)
auto1 d = smartTraverse @(MonoMatch D) (monoApp testFun1) (generateA d)
auto2 d = smartTraverse @(MonoMatch C) (monoApp testFun2) (generateA d)

topDown1 d = topDown @(MonoMatch D) (monoApp testFun1) (generateA d)
topDown2 d = topDown @(MonoMatch C) (monoApp testFun2) (generateA d)
topDownSelective1 d = selectiveTraverse @(MonoMatch D) (\e -> (monoApp testFun1 e, True)) (generateA d)
topDownSelective2 d = selectiveTraverse @(MonoMatch C) (\e -> (monoApp testFun2 e, True)) (generateA d)

bottomUp1 d = bottomUpTrav @(MonoMatch D) (monoApp testFun1) (generateA d)
bottomUp2 d = bottomUpTrav @(MonoMatch C) (monoApp testFun2) (generateA d)


main = defaultMain
  [ bench "generation" $ nf generateA 141
  , bgroup "our"
    [ bench "deep" $ nf our1 141
    , bench "shallow" $ nf our2 141
    ]
  , bgroup "topdown"
    [ bench "deep" $ nf topDown1 141
    , bench "shallow" $ nf topDown2 141
    ]
  , bgroup "selective"
    [ bench "deep" $ nf topDownSelective1 141
    , bench "shallow" $ nf topDownSelective2 141
    ]
  , bgroup "bottomUp-desc"
    [ bench "deep" $ nf bottomUp1 141
    , bench "shallow" $ nf bottomUp2 141
    ]
  , bgroup "auto"
    [ bench "deep" $ nf auto1 141
    , bench "shallow" $ nf auto2 141
    ]
  , bgroup "autoUni" 
    [ bench "deep" $ nf autoUni1 141
    , bench "shallow" $ nf autoUni2 141
    ]
  , bgroup "directUni" 
    [ bench "deep" $ nf directUni1 141
    , bench "shallow" $ nf directUni2 141
    ]
  ]