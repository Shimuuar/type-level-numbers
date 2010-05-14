{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls        #-}
module Types.Boolean ( True
                     , False
                       -- * Boolean operations
                     , NotT(..)
                     , notT
                     , AndT(..)
                     , andT
                     , OrT(..)
                     , orT
                     , XorT(..)
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
class NotT a where
  type Not a :: *

notT :: NotT a => a -> Not a
notT _ = undefined

instance NotT False where type Not False = True
instance NotT True  where type Not True  = False

----------------------------------------------------------------
-- | And for boolean types
class AndT a b where
  type And a b :: *

andT :: AndT a b => a -> b -> And a b
andT _ _ = undefined

instance AndT False False where type And False False = False
instance AndT False True  where type And False True  = False
instance AndT True  False where type And True  False = False
instance AndT True  True  where type And True  True  = True

----------------------------------------------------------------
-- | Or for boolean types
class OrT a b where
  type Or a b :: *

orT :: OrT a b => a -> b -> Or a b
orT _ _ = undefined

instance OrT False False where type Or False False = False
instance OrT False True  where type Or False True  = False
instance OrT True  False where type Or True  False = False
instance OrT True  True  where type Or True  True  = True

----------------------------------------------------------------
-- | Exlusive or for boolean types
class XorT a b where
  type Xor a b :: *

xorT :: XorT a b => a -> b -> Xor a b
xorT _ _ = undefined

instance XorT False False where type Xor False False = False
instance XorT False True  where type Xor False True  = False
instance XorT True  False where type Xor True  False = False
instance XorT True  True  where type Xor True  True  = True
