module AutoUni where

import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data

import Generate

autoUni d = transformBi testFun (generateA d)

