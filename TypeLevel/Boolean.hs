{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE EmptyDataDecls #-}
module TypeLevel.Boolean ( True
                     , False
                       -- * Boolean operations
                     , Not
                     , notT
                     , And
                     , andT
                     , Or
                     , orT
                     , Xor
                     , xorT
                     ) where


-- | Data type for truth
data True
-- | Data type for false.
data False

instance Show False where show _ = "False"
instance Show True  where show _ = "True"

----------------------------------------------------------------
-- | Negation

type family Not a :: *

notT :: a -> Not a
notT _ = undefined

type instance Not False = True
type instance Not True  = False

----------------------------------------------------------------
-- | And for boolean types
type family And a b :: *

andT :: a -> b -> And a b
andT _ _ = undefined

type instance And False False = False
type instance And False True  = False
type instance And True  False = False
type instance And True  True  = True

----------------------------------------------------------------
-- | Or for boolean types
type family Or a b :: *

orT :: a -> b -> Or a b
orT _ _ = undefined

type instance Or False False = True
type instance Or False True  = True
type instance Or True  False = True
type instance Or True  True  = False

----------------------------------------------------------------
-- | Exlusive or for boolean types
type family Xor a b :: *

xorT :: a -> b -> Xor a b
xorT _ _ = undefined

type instance Xor False False = False
type instance Xor False True  = True
type instance Xor True  False = True
type instance Xor True  True  = False
