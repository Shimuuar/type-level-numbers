{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Types.Number.Int
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
--
-- Type level signed integer numbers are implemented using balanced
-- ternary encoding much in the same way as natural numbers.
--
-- Currently following operations are supported: Next, Prev, Add, Sub,
-- Mul.
module Types.Number.Int ( -- * Integer numbers
                          ZZ
                        , Dn
                        , D0
                        , D1
                        , IntT
                          -- * Template haskell utilities
                        , intT
                        , module Types.Number.Classes
                        ) where

import Language.Haskell.TH

import Types.Number.Classes
import Types.Number.Int.Types
import Types.Util


splitToTrits :: Integer -> [Int]
splitToTrits 0 = []
splitToTrits x | n == 0 =  0 : splitToTrits  rest
               | n == 1 =  1 : splitToTrits  rest
               | n == 2 = -1 : splitToTrits (rest + 1)
                 where
                   (rest,n) = divMod x 3

-- | Generate type for integer number.
intT :: Integer -> TypeQ
intT = foldr appT [t| ZZ |] . map con . splitToTrits
  where
    con (-1) = [t| Dn |]
    con   0 = [t| D0 |]
    con   1 = [t| D1 |]
    con   x = error $ "Strange trit: " ++ show x

----------------------------------------------------------------
--
instance TypeInt     ZZ  where toInt _ =  0
instance TypeInt (D1 ZZ) where toInt _ =  1
instance TypeInt (Dn ZZ) where toInt _ = -1

instance TypeInt (Dn n) => TypeInt (Dn (Dn n)) where toInt n = -1 + 3 * toInt' n
instance TypeInt (Dn n) => TypeInt (D0 (Dn n)) where toInt n =  0 + 3 * toInt' n
instance TypeInt (Dn n) => TypeInt (D1 (Dn n)) where toInt n =  1 + 3 * toInt' n
instance TypeInt (D0 n) => TypeInt (Dn (D0 n)) where toInt n = -1 + 3 * toInt' n
instance TypeInt (D0 n) => TypeInt (D0 (D0 n)) where toInt n =  0 + 3 * toInt' n
instance TypeInt (D0 n) => TypeInt (D1 (D0 n)) where toInt n =  1 + 3 * toInt' n
instance TypeInt (D1 n) => TypeInt (Dn (D1 n)) where toInt n = -1 + 3 * toInt' n
instance TypeInt (D1 n) => TypeInt (D0 (D1 n)) where toInt n =  0 + 3 * toInt' n
instance TypeInt (D1 n) => TypeInt (D1 (D1 n)) where toInt n =  1 + 3 * toInt' n

toInt' :: (TypeInt a, Integral i) => t a -> i
toInt' = toInt . cdr

-- | Type class for type level integers. Only numbers without leading
-- zeroes are members of the class.
class IntT n where

instance                   IntT     ZZ
instance TypeInt (Dn n) => IntT (Dn n)
instance TypeInt (D0 n) => IntT (D0 n)
instance TypeInt (D1 n) => IntT (D1 n)

instance                   Show       ZZ   where show _ = "[0]:Z"
instance TypeInt (Dn n) => Show (Dn n) where show n = "["++show (toInt n)++":Z]"
instance TypeInt (D0 n) => Show (D0 n) where show n = "["++show (toInt n)++":Z]"
instance TypeInt (D1 n) => Show (D1 n) where show n = "["++show (toInt n)++":Z]"

----------------------------------------------------------------
-- Number normalization

type family   AddBit n :: *
type instance AddBit    ZZ = ZZ
type instance AddBit (a b) = D0 (a b)

type instance Normalized     ZZ = ZZ
type instance Normalized (Dn n) = Dn     (Normalized n)
type instance Normalized (D0 n) = AddBit (Normalized n)
type instance Normalized (D1 n) = D1     (Normalized n)

----------------------------------------------------------------
-- Next Number
instance                NextN     ZZ where type Next     ZZ = D1 ZZ
instance IntT (Dn n) => NextN (Dn n) where type Next (Dn n) = Normalized (D0 n)
instance IntT (D0 n) => NextN (D0 n) where type Next (D0 n) = D1 n
instance IntT (D1 n) => NextN (D1 n) where type Next (D1 n) = Normalized (Dn (Next n))

----------------------------------------------------------------
-- Previous number
instance                PrevN     ZZ where type Prev     ZZ = Dn ZZ
instance IntT (Dn n) => PrevN (Dn n) where type Prev (Dn n) = Normalized (D1 (Prev n))
instance IntT (D0 n) => PrevN (D0 n) where type Prev (D0 n) = Dn n
instance IntT (D1 n) => PrevN (D1 n) where type Prev (D1 n) = Normalized (D0 n)

----------------------------------------------------------------
-- Negate number
instance NegateN ZZ where type Negate ZZ = ZZ
instance NegateN (Dn n) where type Negate (Dn n) = D1 (Negate n)
instance NegateN (D0 n) where type Negate (D0 n) = D0 (Negate n)
instance NegateN (D1 n) where type Negate (D1 n) = Dn (Negate n)


----------------------------------------------------------------
-- Addition

-- Type class which actually implement addtition of natural numbers
type family Add' n m carry :: *

data CarryN
data Carry0
data Carry1

-- Special cases with ZZ
type instance Add'     ZZ     ZZ Carry0 = ZZ
type instance Add'     ZZ (Dn n) Carry0 = (Dn n)
type instance Add'     ZZ (D0 n) Carry0 = (D0 n)
type instance Add'     ZZ (D1 n) Carry0 = (D1 n)
type instance Add' (Dn n)     ZZ Carry0 = (Dn n)
type instance Add' (D0 n)     ZZ Carry0 = (D0 n)
type instance Add' (D1 n)     ZZ Carry0 = (D1 n)
--
type instance Add'     ZZ     ZZ CarryN = Dn ZZ
type instance Add'     ZZ (Dn n) CarryN = Prev (Dn n)
type instance Add'     ZZ (D0 n) CarryN = (Dn n)
type instance Add'     ZZ (D1 n) CarryN = (D0 n)
type instance Add' (Dn n)     ZZ CarryN = Prev (Dn n)
type instance Add' (D0 n)     ZZ CarryN = (Dn n)
type instance Add' (D1 n)     ZZ CarryN = (D0 n)
--
type instance Add'     ZZ     ZZ Carry1 = D1 ZZ
type instance Add'     ZZ (Dn n) Carry1 = (D0 n)
type instance Add'     ZZ (D0 n) Carry1 = (D1 n)
type instance Add'     ZZ (D1 n) Carry1 = Next (D1 n)
type instance Add' (Dn n)     ZZ Carry1 = (D0 n)
type instance Add' (D0 n)     ZZ Carry1 = (D1 n)
type instance Add' (D1 n)     ZZ Carry1 = Next (D1 n)

-- == General recursion ==
-- No carry
type instance Add' (Dn n) (Dn m) Carry0 = D1 (Add' n m CarryN)
type instance Add' (D0 n) (Dn m) Carry0 = Dn (Add' n m Carry0)
type instance Add' (D1 n) (Dn m) Carry0 = D0 (Add' n m Carry0)
--
type instance Add' (Dn n) (D0 m) Carry0 = Dn (Add' n m Carry0)
type instance Add' (D0 n) (D0 m) Carry0 = D0 (Add' n m Carry0)
type instance Add' (D1 n) (D0 m) Carry0 = D1 (Add' n m Carry0)
--
type instance Add' (Dn n) (D1 m) Carry0 = D0 (Add' n m Carry0)
type instance Add' (D0 n) (D1 m) Carry0 = D1 (Add' n m Carry0)
type instance Add' (D1 n) (D1 m) Carry0 = Dn (Add' n m Carry1)
-- Carry '-'
type instance Add' (Dn n) (Dn m) CarryN = D0 (Add' n m CarryN)
type instance Add' (D0 n) (Dn m) CarryN = D1 (Add' n m CarryN)
type instance Add' (D1 n) (Dn m) CarryN = Dn (Add' n m Carry0)
--
type instance Add' (Dn n) (D0 m) CarryN = D1 (Add' n m CarryN)
type instance Add' (D0 n) (D0 m) CarryN = Dn (Add' n m Carry0)
type instance Add' (D1 n) (D0 m) CarryN = D0 (Add' n m Carry0)
--
type instance Add' (Dn n) (D1 m) CarryN = Dn (Add' n m Carry0)
type instance Add' (D0 n) (D1 m) CarryN = D0 (Add' n m Carry0)
type instance Add' (D1 n) (D1 m) CarryN = D1 (Add' n m Carry0)
-- Carry '+'
type instance Add' (Dn n) (Dn m) Carry1 = Dn (Add' n m Carry0)
type instance Add' (D0 n) (Dn m) Carry1 = D0 (Add' n m Carry0)
type instance Add' (D1 n) (Dn m) Carry1 = D1 (Add' n m Carry0)
--
type instance Add' (Dn n) (D0 m) Carry1 = D0 (Add' n m Carry0)
type instance Add' (D0 n) (D0 m) Carry1 = D1 (Add' n m Carry0)
type instance Add' (D1 n) (D0 m) Carry1 = Dn (Add' n m Carry1)
--
type instance Add' (Dn n) (D1 m) Carry1 = D0 (Add' n m Carry0)
type instance Add' (D0 n) (D1 m) Carry1 = D1 (Add' n m Carry1)
type instance Add' (D1 n) (D1 m) Carry1 = Dn (Add' n m Carry1)

-- Instances for AddN
instance                               AddN     ZZ     ZZ where type Add     ZZ     ZZ = ZZ
instance (IntT (Dn n))              => AddN     ZZ (Dn n) where type Add     ZZ (Dn n) = (Dn n)
instance (IntT (D0 n))              => AddN     ZZ (D0 n) where type Add     ZZ (D0 n) = (D0 n)
instance (IntT (D1 n))              => AddN     ZZ (D1 n) where type Add     ZZ (D1 n) = (D1 n)
instance (IntT (Dn n))              => AddN (Dn n)     ZZ where type Add (Dn n)     ZZ = (Dn n)
instance (IntT (D0 n))              => AddN (D0 n)     ZZ where type Add (D0 n)     ZZ = (D0 n)
instance (IntT (D1 n))              => AddN (D1 n)     ZZ where type Add (D1 n)     ZZ = (D1 n)
--
instance (IntT (Dn n), IntT (Dn m)) => AddN (Dn n) (Dn m) where type Add (Dn n) (Dn m) = Normalized (Add' (Dn n) (Dn m) Carry0)
instance (IntT (D0 n), IntT (Dn m)) => AddN (D0 n) (Dn m) where type Add (D0 n) (Dn m) = Normalized (Add' (D0 n) (Dn m) Carry0)
instance (IntT (D1 n), IntT (Dn m)) => AddN (D1 n) (Dn m) where type Add (D1 n) (Dn m) = Normalized (Add' (D1 n) (Dn m) Carry0)
--
instance (IntT (Dn n), IntT (D0 m)) => AddN (Dn n) (D0 m) where type Add (Dn n) (D0 m) = Normalized (Add' (Dn n) (D0 m) Carry0)
instance (IntT (D0 n), IntT (D0 m)) => AddN (D0 n) (D0 m) where type Add (D0 n) (D0 m) = Normalized (Add' (D0 n) (D0 m) Carry0)
instance (IntT (D1 n), IntT (D0 m)) => AddN (D1 n) (D0 m) where type Add (D1 n) (D0 m) = Normalized (Add' (D1 n) (D0 m) Carry0)
--
instance (IntT (Dn n), IntT (D1 m)) => AddN (Dn n) (D1 m) where type Add (Dn n) (D1 m) = Normalized (Add' (Dn n) (D1 m) Carry0)
instance (IntT (D0 n), IntT (D1 m)) => AddN (D0 n) (D1 m) where type Add (D0 n) (D1 m) = Normalized (Add' (D0 n) (D1 m) Carry0)
instance (IntT (D1 n), IntT (D1 m)) => AddN (D1 n) (D1 m) where type Add (D1 n) (D1 m) = Normalized (Add' (D1 n) (D1 m) Carry0)


----------------------------------------------------------------
-- Subtraction.
--
-- Subtraction is much easier since is ise defined using
-- addition and negation

instance                               SubN     ZZ     ZZ where type Sub     ZZ     ZZ = ZZ
instance (IntT (Dn n))              => SubN     ZZ (Dn n) where type Sub     ZZ (Dn n) = Negate (Dn n)
instance (IntT (D0 n))              => SubN     ZZ (D0 n) where type Sub     ZZ (D0 n) = Negate (D0 n)
instance (IntT (D1 n))              => SubN     ZZ (D1 n) where type Sub     ZZ (D1 n) = Negate (D1 n)
instance (IntT (Dn n))              => SubN (Dn n)     ZZ where type Sub (Dn n)     ZZ = (Dn n)
instance (IntT (D0 n))              => SubN (D0 n)     ZZ where type Sub (D0 n)     ZZ = (D0 n)
instance (IntT (D1 n))              => SubN (D1 n)     ZZ where type Sub (D1 n)     ZZ = (D1 n)

instance (IntT (Dn n), IntT (Dn m)) => SubN (Dn n) (Dn m) where type Sub (Dn n) (Dn m) = Add (Dn n) (Negate (Dn m))
instance (IntT (D0 n), IntT (Dn m)) => SubN (D0 n) (Dn m) where type Sub (D0 n) (Dn m) = Add (D0 n) (Negate (Dn m))
instance (IntT (D1 n), IntT (Dn m)) => SubN (D1 n) (Dn m) where type Sub (D1 n) (Dn m) = Add (D1 n) (Negate (Dn m))
--
instance (IntT (Dn n), IntT (D0 m)) => SubN (Dn n) (D0 m) where type Sub (Dn n) (D0 m) = Add (Dn n) (Negate (D0 m))
instance (IntT (D0 n), IntT (D0 m)) => SubN (D0 n) (D0 m) where type Sub (D0 n) (D0 m) = Add (D0 n) (Negate (D0 m))
instance (IntT (D1 n), IntT (D0 m)) => SubN (D1 n) (D0 m) where type Sub (D1 n) (D0 m) = Add (D1 n) (Negate (D0 m))
--
instance (IntT (Dn n), IntT (D1 m)) => SubN (Dn n) (D1 m) where type Sub (Dn n) (D1 m) = Add (Dn n) (Negate (D1 m))
instance (IntT (D0 n), IntT (D1 m)) => SubN (D0 n) (D1 m) where type Sub (D0 n) (D1 m) = Add (D0 n) (Negate (D1 m))
instance (IntT (D1 n), IntT (D1 m)) => SubN (D1 n) (D1 m) where type Sub (D1 n) (D1 m) = Add (D1 n) (Negate (D1 m))


----------------------------------------------------------------
-- Multiplication

instance (IntT n)              => MulN n    ZZ  where type Mul n    ZZ  = ZZ
instance (IntT n, IntT (Dn m)) => MulN n (Dn m) where type Mul n (Dn m) = Normalized (Add' (Negate n) (D0 (Mul n m)) Carry0)
instance (IntT n, IntT (D0 m)) => MulN n (D0 m) where type Mul n (D0 m) = Normalized (D0 (Mul n m))
instance (IntT n, IntT (D1 m)) => MulN n (D1 m) where type Mul n (D1 m) = Normalized (Add'         n  (D0 (Mul n m)) Carry0)
