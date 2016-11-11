{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module DirectUni where

import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Direct

import Example
import Generate

-- data A = ABC B C deriving (Show, Generic, Data)
-- data B = B | BA A deriving (Show, Generic, Data)
-- data C = CB B | CD D deriving (Show, Generic, Data)
-- data D = DDE D E | D deriving (Show, Generic, Data)
-- data E = E deriving (Show, Generic, Data)

instance Uniplate A where
    uniplate (ABC b c) = plate ABC |+ b |+ c

instance Biplate A B where
    biplate (ABC b c) = plate ABC |+ b |+ c

instance Biplate A C where
    biplate (ABC b c) = plate ABC |+ b |+ c

instance Biplate A D where
    biplate (ABC b c) = plate ABC |+ b |+ c

instance Biplate A E where
    biplate (ABC b c) = plate ABC |+ b |+ c


instance Uniplate B where
    uniplate (BA a) = plate BA |+ a
    uniplate B = plate B

instance Biplate B A where
    biplate (BA a) = plate BA |* a
    biplate B = plate B

instance Biplate B C where
    biplate (BA a) = plate BA |+ a
    biplate B = plate B

instance Biplate B D where
    biplate (BA a) = plate BA |+ a
    biplate B = plate B

instance Biplate B E where
    biplate (BA a) = plate BA |+ a
    biplate B = plate B


instance Uniplate C where
    uniplate (CB b) = plate CB |+ b
    uniplate (CD d) = plate CD |- d

instance Biplate C A where
    biplate (CB b) = plate CB |+ b
    biplate (CD d) = plate CD |- d

instance Biplate C B where
    biplate (CB b) = plate CB |+ b
    biplate (CD d) = plate CD |- d

instance Biplate C D where
    biplate (CB b) = plate CB |+ b
    biplate (CD d) = plate CD |+ d

instance Biplate C E where
    biplate (CB b) = plate CB |+ b
    biplate (CD d) = plate CD |+ d

instance Uniplate D where
    uniplate (DDE d e) = plate DDE |* d |- e 
    uniplate D = plate D

instance Biplate D A where
    biplate (DDE d e) = plate DDE |- d |- e 
    biplate D = plate D

instance Biplate D B where
    biplate (DDE d e) = plate DDE |- d |- e 
    biplate D = plate D

instance Biplate D C where
    biplate (DDE d e) = plate DDE |- d |- e 
    biplate D = plate D

instance Biplate D E where
    biplate (DDE d e) = plate DDE |+ d |* e 
    biplate D = plate D

instance Uniplate E where
    uniplate E = plate E 

instance Biplate E A where
    biplate E = plate E 

instance Biplate E B where
    biplate E = plate E 

instance Biplate E C where
    biplate E = plate E 

instance Biplate E D where
    biplate E = plate E 


instance Biplate A A where
    biplate = plateSelf

instance Biplate B B where
    biplate = plateSelf

instance Biplate C C where
    biplate = plateSelf

instance Biplate D D where
    biplate = plateSelf

instance Biplate E E where
    biplate = plateSelf


directUni1 d = transformBi testFun1 (generateA d)
directUni2 d = transformBi testFun2 (generateA d)

