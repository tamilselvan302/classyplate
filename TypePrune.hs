{-# LANGUAGE KindSignatures
           , TypeOperators
           , DataKinds
           , PolyKinds
           , TypeFamilies
           , UndecidableInstances
           #-}
module TypePrune (ClassIgnoresSubtree, AppSelector) where

import GHC.Exts (Constraint)
import GHC.Generics
import Data.Type.Bool
import Data.Type.List hiding (Distinct)

-- | This type decides if the subtree of an element cannot contain an element that is transformed.
type family ClassIgnoresSubtree (cls :: * -> Constraint) (typ :: *) :: Bool where
  ClassIgnoresSubtree cls typ = Not (AnySelected cls (MemberTypes typ))

-- | Instantiate this type family to signal what elements does your operation operate on.
-- If @AppSelector c t@ is True, there should be a @c t@ instance. AppSelector should be
-- a total type function for a given class, at least for all the types that can possibly
-- accessed.
type family AppSelector (c :: * -> Constraint) (a :: *) :: Bool

type family AnySelected (c :: * -> Constraint) (ls :: [*]) :: Bool where
  AnySelected c (fst ': rest) = AppSelector c fst || AnySelected c rest
  AnySelected c '[] = False

type family Distinct (ls1 :: [k]) (ls2 :: [k]) :: Bool where
  Distinct '[] ls2 = True
  Distinct ls1 '[] = True
  Distinct (e1 ': ls1) ls2 = Not (Find e1 ls2) && Distinct ls1 ls2

type family MemberTypes (typ :: *) :: [*] where
  MemberTypes t = GetMemberTypes '[] t

type family GetMemberTypes (checked :: [*]) (typ :: *) :: [*] where 
  -- primitive types without Rep instance
  GetMemberTypes checked Char = '[Char]
  GetMemberTypes checked Int = '[Int]
  GetMemberTypes checked Float = '[Float]
  GetMemberTypes checked Double = '[Double]
  GetMemberTypes checked t = GetElementTypes checked (Rep t)

type family GetElementTypes (checked :: [*]) (typ :: * -> *) :: [*] where 
  GetElementTypes checked (D1 md cons) = GetElementTypesCons checked cons

type family GetElementTypesCons (checked :: [*]) (typ :: * -> *) where 
  GetElementTypesCons checked (C1 mc flds) = GetElementTypesFields checked flds
  GetElementTypesCons checked (c1 :+: c2) = GetElementTypesCons checked c1 `Union` GetElementTypesCons checked c2

type family GetElementTypesFields (checked :: [*]) (typ :: * -> *) where 
  GetElementTypesFields checked (fld1 :*: fld2) = GetElementTypesFields checked fld1 `Union` GetElementTypesFields checked fld2
  GetElementTypesFields checked (S1 ms (Rec0 t)) = GetElementTypesField checked (Find t checked) t
  GetElementTypesFields checked U1 = '[]

type family GetElementTypesField (checked :: [*]) (inChecked :: Bool) (typ :: *) where 
  GetElementTypesField checked True typ = '[]
  GetElementTypesField checked False typ = Insert typ (GetMemberTypes (typ ': checked) typ)
