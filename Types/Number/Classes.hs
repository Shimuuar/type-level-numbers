{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Types.Number.Classes ( -- * Comparison of numbers
                              CompareN(..)
                            , compareN
                              -- ** Data labels for comparison
                            , IsLesser
                            , IsEqual
                            , IsGreater
                              -- ** Specialized type classes
                            , Lesser
                            , LesserEq
                              -- * Arithmetic operations on numbers
                            , NextN(..)
                            , nextN
                            , PrevN(..)
                            , prevN
                            , AddN(..)
                            , addN
                            , SubN(..)
                            , subN
                            , MulN(..)
                            , mulN
                            ) where

----------------------------------------------------------------
-- Comparison
----------------------------------------------------------------

-- | Type class for comparing two numbers. It's expected that for any
-- two valid n and m Compare n m is equal to IsLess when n<m, IsEqual
-- when n=m and IsGreater when n>m.
class CompareN n m where
    type Compare n m :: *

compareN :: CompareN n m => n -> m -> Compare n m
compareN _ _ = undefined

data IsLesser
data IsEqual
data IsGreater

instance Show IsLesser  where show _  = "IsLesser"
instance Show IsEqual   where show _  = "IsEqual"
instance Show IsGreater where show _  = "IsGreater"

----------------------------------------------------------------

-- | Numbers n and m are instances of this class if and only is n < m.
class Lesser n m

-- | Numbers n and m are instances of this class if and only is n > m.
class Greater n m

-- | Numbers n and m are instances of this class if and only is n <= m.
class LesserEq n m

-- | Numbers n and m are instances of this class if and only is n >= m.
class GreaterEq n m

-- a b c are instance of class only when a ~ b or a ~ c. Require ovelapping.
class    OneOfTwo a b c
instance OneOfTwo a a b
instance OneOfTwo a b a
instance OneOfTwo a a a

instance (Compare n m ~ IsLesser ) => Lesser n m
instance (Compare n m ~ IsGreater) => Greater n m
-- Instances for LessEq and GreaterEq are tricker.
instance (OneOfTwo (Compare n m) IsLesser  IsEqual) => LesserEq n m
instance (OneOfTwo (Compare n m) IsGreater IsEqual) => GreaterEq n m

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Next number.
class NextN n where
    type Next n :: *

nextN :: NextN n => n -> Next n
nextN _ = undefined

-- | Previous number.
class PrevN n where
    type Prev n :: *

prevN :: PrevN n => n -> Prev n
prevN _ = undefined

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Sum of two numbers.
class AddN n m where
    type Add n m :: *

addN :: AddN n m => n -> m -> Add n m
addN _ _ = undefined

-- | Difference of two numbers.
class SubN n m where
    type Sub n m :: *

subN :: SubN n m => n -> m -> Sub n m
subN _ _ = undefined

-- | Product.
class MulN n m where
  type Mul n m :: *
       
mulN :: MulN n m => n -> m -> Mul n m
mulN _ _ = undefined