{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Types.Number.Classes ( -- * Conversion to values
                              TypeInt(..)
                              -- * Comparison of numbers
                            , CompareN(..)
                            , compareN
                              -- ** Data labels for types comparison
                            , IsLesser
                            , IsEqual
                            , IsGreater
                              -- ** Specialized type classes
                              -- $comparing
                            , Lesser
                            , LesserEq
                            , Greater
                            , GreaterEq
                              -- * Arithmetic operations on numbers
                            , NextN(..)
                            , nextN
                            , PrevN(..)
                            , prevN
                            , NegateN(..)
                            , negateN
                            , AddN(..)
                            , addN
                            , SubN(..)
                            , subN
                            , MulN(..)
                            , mulN
                            , DivN(..)
                            , divN
                              -- * Special classes
                            , Normalized
                            ) where

-- | Type class for conversion type level integral numbers to value
-- level numbers.
class TypeInt n where
    -- | This function is expected to be completely lazy in its argument.
    toInt :: Integral i => n -> i

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

-- $comparing
-- These type classes are meant to be used in contexts to ensure
-- relations between numbers. For example:
-- 
-- > someFunction :: Lesser n m => Data n -> Data m -> Data n
-- > someFunction = ...
--
-- They have generic instances and every number which is instance of
-- CompareN type class is instance of these type classes.

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
-- Instances for LessEq and GreaterEq are trickier.
instance (OneOfTwo (Compare n m) IsLesser  IsEqual) => LesserEq n m
instance (OneOfTwo (Compare n m) IsGreater IsEqual) => GreaterEq n m

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

-- | Negate numbers
class NegateN n where
    type Negate n :: *

negateN :: NegateN n => n -> Negate n
negateN _ = undefined

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

-- | Division of two numbers. 'n' and 'm' should be instances of this
-- class only if remainder of 'n/m' is zero.
class DivN n m where
    type Div n m :: *

divN :: DivN n m => n -> m -> Div n m
divN _ _ = undefined

----------------------------------------------------------------

-- | Usually numbers have non-unique representation. This type family
-- is canonical representation of number.
type family Normalized n :: *
