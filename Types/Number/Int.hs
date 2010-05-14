{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Types.Number.Int ( ZZ
                        , D1
                        , D0
                        , Dn
                        , IntT(..)
                        , module Types.Number.Classes
                        ) where

import Types.Number.Classes

----------------------------------------------------------------
data D1 n
data D0 n
data Dn n
data ZZ
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

----------------------------------------------------------------
instance                NextN     ZZ where type Next     ZZ = D1 ZZ
instance IntT (Dn n) => NextN (Dn n) where type Next (Dn n) = Normalized (D0 n)
instance IntT (D0 n) => NextN (D0 n) where type Next (D0 n) = D1 n
instance IntT (D1 n) => NextN (D1 n) where type Next (D1 n) = Normalized (Dn (Next n))

instance                PrevN     ZZ where type Prev     ZZ = Dn ZZ
instance IntT (Dn n) => PrevN (Dn n) where type Prev (Dn n) = Normalized (D1 (Prev n))
instance IntT (D0 n) => PrevN (D0 n) where type Prev (D0 n) = Dn n
instance IntT (D1 n) => PrevN (D1 n) where type Prev (D1 n) = Normalized (D0 n)


----------------------------------------------------------------
-- ================================================================
----------------------------------------------------------------

z :: ZZ
z = undefined
