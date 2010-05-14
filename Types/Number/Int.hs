{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
module Types.Number.Int ( ZZ
                        , D1
                        , D0
                        , Dn
                        , IntT(..)
                        , module Types.Number.Classes
                        ) where

import Types.Number.Classes
import Types.Number.Int.Types

import Language.Haskell.TH

splitToTrits :: Integer -> [Int]
splitToTrits 0 = []
splitToTrits x | n == 0 =  0 : splitToTrits  rest
               | n == 1 =  1 : splitToTrits  rest
               | n == 2 = -1 : splitToTrits (rest + 1)
                 where   
                   (rest,n) = divMod x 3

intT :: Integer -> TypeQ
intT = foldr appT [t| ZZ |] . map con . splitToTrits
  where
    con (-1) = [t| Dn |]
    con   0 = [t| D0 |]
    con   1 = [t| D1 |]
    con   x = error $ "Strange trit: " ++ show x

----------------------------------------------------------------
class IntT n where
  toIntZ :: n -> Int

instance IntT     ZZ  where toIntZ _ =  0
instance IntT (D1 ZZ) where toIntZ _ =  1
instance IntT (Dn ZZ) where toIntZ _ = -1

instance IntT (Dn n) => IntT (Dn (Dn n)) where toIntZ n = -1 + 3 * toIntZ' n
instance IntT (Dn n) => IntT (D0 (Dn n)) where toIntZ n =  0 + 3 * toIntZ' n
instance IntT (Dn n) => IntT (D1 (Dn n)) where toIntZ n =  1 + 3 * toIntZ' n
instance IntT (D0 n) => IntT (Dn (D0 n)) where toIntZ n = -1 + 3 * toIntZ' n
instance IntT (D0 n) => IntT (D0 (D0 n)) where toIntZ n =  0 + 3 * toIntZ' n
instance IntT (D0 n) => IntT (D1 (D0 n)) where toIntZ n =  1 + 3 * toIntZ' n
instance IntT (D1 n) => IntT (Dn (D1 n)) where toIntZ n = -1 + 3 * toIntZ' n
instance IntT (D1 n) => IntT (D0 (D1 n)) where toIntZ n =  0 + 3 * toIntZ' n
instance IntT (D1 n) => IntT (D1 (D1 n)) where toIntZ n =  1 + 3 * toIntZ' n

toIntZ' :: (IntT a) => t a -> Int
toIntZ' = toIntZ . cdr

cdr :: t a -> a
cdr _ = undefined

instance              Show       ZZ   where show _ = "[0]:Z"
instance IntT (Dn n) => Show (Dn n) where show n = "["++show (toIntZ n)++"]:Z"
instance IntT (D0 n) => Show (D0 n) where show n = "["++show (toIntZ n)++"]:Z"
instance IntT (D1 n) => Show (D1 n) where show n = "["++show (toIntZ n)++"]:Z"

----------------------------------------------------------------
-- Number normalization

class    NormBit    n  where type AddBit n :: *
instance NormBit    ZZ where type AddBit    ZZ = ZZ
instance NormBit (a b) where type AddBit (a b) = D0 (a b)

instance NormalizedNumber     ZZ where type Normalized     ZZ = ZZ
instance NormalizedNumber (Dn n) where type Normalized (Dn n) = Dn     (Normalized n)
instance NormalizedNumber (D0 n) where type Normalized (D0 n) = AddBit (Normalized n)
instance NormalizedNumber (D1 n) where type Normalized (D1 n) = D1     (Normalized n)

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
class AddN' n m carry where
    type Add' n m carry :: *

data CarryN
data Carry0
data Carry1

-- Special cases with ZZ
instance AddN'     ZZ     ZZ Carry0 where type Add'     ZZ     ZZ Carry0 = ZZ
instance AddN'     ZZ (Dn n) Carry0 where type Add'     ZZ (Dn n) Carry0 = (Dn n)
instance AddN'     ZZ (D0 n) Carry0 where type Add'     ZZ (D0 n) Carry0 = (D0 n)
instance AddN'     ZZ (D1 n) Carry0 where type Add'     ZZ (D1 n) Carry0 = (D1 n)
instance AddN' (Dn n)     ZZ Carry0 where type Add' (Dn n)     ZZ Carry0 = (Dn n)
instance AddN' (D0 n)     ZZ Carry0 where type Add' (D0 n)     ZZ Carry0 = (D0 n)
instance AddN' (D1 n)     ZZ Carry0 where type Add' (D1 n)     ZZ Carry0 = (D1 n)
--
instance AddN'     ZZ     ZZ CarryN where type Add'     ZZ     ZZ CarryN = Dn ZZ
instance AddN'     ZZ (Dn n) CarryN where type Add'     ZZ (Dn n) CarryN = Prev (Dn n)
instance AddN'     ZZ (D0 n) CarryN where type Add'     ZZ (D0 n) CarryN = (Dn n)
instance AddN'     ZZ (D1 n) CarryN where type Add'     ZZ (D1 n) CarryN = (D0 n)
instance AddN' (Dn n)     ZZ CarryN where type Add' (Dn n)     ZZ CarryN = Prev (Dn n)
instance AddN' (D0 n)     ZZ CarryN where type Add' (D0 n)     ZZ CarryN = (Dn n)
instance AddN' (D1 n)     ZZ CarryN where type Add' (D1 n)     ZZ CarryN = (D0 n)
--
instance AddN'     ZZ     ZZ Carry1 where type Add'     ZZ     ZZ Carry1 = D1 ZZ
instance AddN'     ZZ (Dn n) Carry1 where type Add'     ZZ (Dn n) Carry1 = (D0 n)
instance AddN'     ZZ (D0 n) Carry1 where type Add'     ZZ (D0 n) Carry1 = (D1 n)
instance AddN'     ZZ (D1 n) Carry1 where type Add'     ZZ (D1 n) Carry1 = Next (D1 n)
instance AddN' (Dn n)     ZZ Carry1 where type Add' (Dn n)     ZZ Carry1 = (D0 n)
instance AddN' (D0 n)     ZZ Carry1 where type Add' (D0 n)     ZZ Carry1 = (D1 n)
instance AddN' (D1 n)     ZZ Carry1 where type Add' (D1 n)     ZZ Carry1 = Next (D1 n)

-- == General recursion ==
-- No carry
instance AddN' (Dn n) (Dn m) Carry0 where type Add' (Dn n) (Dn m) Carry0 = D1 (Add' n m CarryN)
instance AddN' (D0 n) (Dn m) Carry0 where type Add' (D0 n) (Dn m) Carry0 = Dn (Add' n m Carry0)
instance AddN' (D1 n) (Dn m) Carry0 where type Add' (D1 n) (Dn m) Carry0 = D0 (Add' n m Carry0)
--
instance AddN' (Dn n) (D0 m) Carry0 where type Add' (Dn n) (D0 m) Carry0 = Dn (Add' n m Carry0)
instance AddN' (D0 n) (D0 m) Carry0 where type Add' (D0 n) (D0 m) Carry0 = D0 (Add' n m Carry0)
instance AddN' (D1 n) (D0 m) Carry0 where type Add' (D1 n) (D0 m) Carry0 = D1 (Add' n m Carry0)
--
instance AddN' (Dn n) (D1 m) Carry0 where type Add' (Dn n) (D1 m) Carry0 = D0 (Add' n m Carry0)
instance AddN' (D0 n) (D1 m) Carry0 where type Add' (D0 n) (D1 m) Carry0 = D1 (Add' n m Carry0)
instance AddN' (D1 n) (D1 m) Carry0 where type Add' (D1 n) (D1 m) Carry0 = Dn (Add' n m Carry1)
-- Carry '-'
instance AddN' (Dn n) (Dn m) CarryN where type Add' (Dn n) (Dn m) CarryN = D0 (Add' n m CarryN)
instance AddN' (D0 n) (Dn m) CarryN where type Add' (D0 n) (Dn m) CarryN = D1 (Add' n m CarryN)
instance AddN' (D1 n) (Dn m) CarryN where type Add' (D1 n) (Dn m) CarryN = Dn (Add' n m Carry0)
--
instance AddN' (Dn n) (D0 m) CarryN where type Add' (Dn n) (D0 m) CarryN = D1 (Add' n m CarryN)
instance AddN' (D0 n) (D0 m) CarryN where type Add' (D0 n) (D0 m) CarryN = Dn (Add' n m Carry0)
instance AddN' (D1 n) (D0 m) CarryN where type Add' (D1 n) (D0 m) CarryN = D0 (Add' n m Carry0)
--
instance AddN' (Dn n) (D1 m) CarryN where type Add' (Dn n) (D1 m) CarryN = Dn (Add' n m Carry0)
instance AddN' (D0 n) (D1 m) CarryN where type Add' (D0 n) (D1 m) CarryN = D0 (Add' n m Carry0)
instance AddN' (D1 n) (D1 m) CarryN where type Add' (D1 n) (D1 m) CarryN = D1 (Add' n m Carry0)
-- Carry '+'
instance AddN' (Dn n) (Dn m) Carry1 where type Add' (Dn n) (Dn m) Carry1 = Dn (Add' n m Carry0)
instance AddN' (D0 n) (Dn m) Carry1 where type Add' (D0 n) (Dn m) Carry1 = D0 (Add' n m Carry0)
instance AddN' (D1 n) (Dn m) Carry1 where type Add' (D1 n) (Dn m) Carry1 = D1 (Add' n m Carry0)
--
instance AddN' (Dn n) (D0 m) Carry1 where type Add' (Dn n) (D0 m) Carry1 = D0 (Add' n m Carry0)
instance AddN' (D0 n) (D0 m) Carry1 where type Add' (D0 n) (D0 m) Carry1 = D1 (Add' n m Carry0)
instance AddN' (D1 n) (D0 m) Carry1 where type Add' (D1 n) (D0 m) Carry1 = Dn (Add' n m Carry1)
--
instance AddN' (Dn n) (D1 m) Carry1 where type Add' (Dn n) (D1 m) Carry1 = D0 (Add' n m Carry0)
instance AddN' (D0 n) (D1 m) Carry1 where type Add' (D0 n) (D1 m) Carry1 = D1 (Add' n m Carry1)
instance AddN' (D1 n) (D1 m) Carry1 where type Add' (D1 n) (D1 m) Carry1 = Dn (Add' n m Carry1)

-- Instances for AddN
instance                               AddN     ZZ     ZZ where type Add     ZZ     ZZ = ZZ
instance (IntT (Dn n))              => AddN     ZZ (Dn n) where type Add     ZZ (Dn n) = (Dn n)
instance (IntT (D0 n))              => AddN     ZZ (D0 n) where type Add     ZZ (D0 n) = (D0 n)
instance (IntT (D1 n))              => AddN     ZZ (D1 n) where type Add     ZZ (D1 n) = (D1 n)
instance (IntT (Dn n))              => AddN (Dn n)     ZZ where type Add (Dn n)     ZZ = (Dn n)
instance (IntT (D0 n))              => AddN (D0 n)     ZZ where type Add (D0 n)     ZZ = (D0 n)
instance (IntT (D1 n))              => AddN (D1 n)     ZZ where type Add (D1 n)     ZZ = (D1 n)


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
instance (IntT n, IntT (Dn m)) => MulN n (Dn m) where type Mul n (Dn m) = Normalized (Add (Negate n) (D0 (Mul n m)))
instance (IntT n, IntT (D0 m)) => MulN n (D0 m) where type Mul n (D0 m) = Normalized (D0 (Mul n m))
instance (IntT n, IntT (D1 m)) => MulN n (D1 m) where type Mul n (D1 m) = Normalized (Add         n  (D0 (Mul n m)))
