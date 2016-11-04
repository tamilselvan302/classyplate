module AutoUni where

import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data

import Generate

autoUni1 d = transformBi testFun1 (generateA d)
autoUni2 d = transformBi testFun2 (generateA d)

