{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Types.Number.Nat
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
--
--
-- This is type level natural numbers. They are represented using
-- binary encoding which means that reasonable large numbers could be
-- represented. With default context stack depth (20) maximal number
-- is 2^18-1 (262143).
--
-- > Z           = 0
-- > I Z         = 1
-- > O (I Z)     = 2
-- > I (I Z)     = 3
-- > O (O (I Z)) = 4
-- > ...
--
-- It's easy to see that representation for each number is not
-- unique. One could add any numbers of leading zeroes:
--
-- > I Z = I (O Z) = I (O (O Z)) = 1
--
-- In order to enforce uniqueness of representation only numbers
-- without leading zeroes are members of Nat type class. This means
-- than types are equal if and only if numbers are equal.
--
-- Natural numbers support comparison and following operations: Next,
-- Prev, Add, Sub, Mul. All operations on numbers return normalized
-- numbers.
--
-- Interface type classes are reexported from Types.Number.Classes
module Types.Number.Nat ( -- * Natural numbers
                          I
                        , O
                        , Z
                        , Nat(..)
                          -- * Template haskell utilities
                          -- $TH
                        , natT
                        , nat
                        , module Types.Number.Classes
                        ) where

import Language.Haskell.TH

import Types.Number.Classes
import Types.Number.Nat.Types
import Types.Util

splitToBits :: Integer -> [Int]
splitToBits 0 = []
splitToBits x | odd x     = 1 : splitToBits rest
              | otherwise = 0 : splitToBits rest
                where rest = x `div` 2

-- $TH
-- Here is usage example for natT:
--
-- > n123 :: $(natT 123)
-- > n123 = undefined
--
-- This require type splices which are supprted by GHC>=6.12.

-- | Create type for natural number.
natT :: Integer -> TypeQ
natT n | n >= 0    = foldr appT [t| Z |] . map con . splitToBits $ n
       | otherwise = error "natT: negative number is supplied"
  where
    con 0 = [t| O |]
    con 1 = [t| I |]
    con _ = error "natT: Strange bit nor 0 nor 1"

-- | Create value for type level natural. Value itself is undefined.
nat :: Integer -> ExpQ
nat n = sigE [|undefined|] (natT n)

----------------------------------------------------------------

-- | Type class for natural numbers. Only numbers without leading
-- zeroes are members of this type class.
class TypeInt n => Nat n

instance                  TypeInt       Z   where toInt _ = 0
instance                  TypeInt    (I Z)  where toInt _ = 1
instance TypeInt (O n) => TypeInt (O (O n)) where toInt n = 0 + 2 * toInt (cdr n)
instance TypeInt (O n) => TypeInt (I (O n)) where toInt n = 1 + 2 * toInt (cdr n)
instance TypeInt (I n) => TypeInt (O (I n)) where toInt n = 0 + 2 * toInt (cdr n)
instance TypeInt (I n) => TypeInt (I (I n)) where toInt n = 1 + 2 * toInt (cdr n)

instance                  Nat    Z
instance TypeInt (O n) => Nat (O n)
instance TypeInt (I n) => Nat (I n)
-- Error reporting
class    Number_Is_Denormalized a
instance (Number_Is_Denormalized Z) => TypeInt (O Z) where
  toInt = error "quench warning"

----------------------------------------------------------------
-- Number normalization

-- Add trailing zero bit to number. It's added only if number is not
-- equal to zero. Actual normalization is done here.
type family   Add0Bit n :: *
type instance Add0Bit    Z  = Z
type instance Add0Bit (a b) = (O (a b))

type instance Normalized    Z  = Z
type instance Normalized (I n) = I (Normalized n)
type instance Normalized (O n) = Add0Bit (Normalized n)

----------------------------------------------------------------
-- Show instances.
-- Nat contexts are used to ensure correctness of numbers.
instance              Show    Z  where show _ = "[0:N]"
instance Nat (O n) => Show (O n) where show n = "["++show (toInt n)++":N]"
instance Nat (I n) => Show (I n) where show n = "["++show (toInt n)++":N]"

----------------------------------------------------------------
-- Next number.
-- Number normalization is not required.
type instance Next    Z  = I Z
type instance Next (I n) = O (Next n)
type instance Next (O n) = I n

----------------------------------------------------------------
-- Previous number.
-- Normalization isn't requred too. It's done manually in (I Z) case.
type instance Prev    (I Z)   = Z
type instance Prev (O (O n))  = I (Prev (O n))
type instance Prev (I (O n))  = O (O n)
type instance Prev (O (I n))  = I (Prev (I n))
type instance Prev (I (I n))  = O (I n)


----------------------------------------------------------------
-- Comparison

-- Join compare results. a is result of comparison of low digits b is
-- result of comparion of higher digits.
type family Join a b :: *

type instance Join IsLesser  IsEqual   = IsLesser
type instance Join IsEqual   IsEqual   = IsEqual
type instance Join IsGreater IsEqual   = IsGreater
type instance Join a         IsLesser  = IsLesser
type instance Join a         IsGreater = IsGreater

-- Instances for comparison
instance              CompareN    Z     Z  where type Compare    Z     Z  = IsEqual
instance Nat (O n) => CompareN (O n)    Z  where type Compare (O n)    Z  = IsGreater
instance Nat (I n) => CompareN (I n)    Z  where type Compare (I n)    Z  = IsGreater
instance Nat (O n) => CompareN    Z  (O n) where type Compare    Z  (O n) = IsLesser
instance Nat (I n) => CompareN    Z  (I n) where type Compare    Z  (I n) = IsLesser

instance (Nat (O n), Nat (O m)) => CompareN (O n) (O m) where type Compare (O n) (O m) = Compare n m
instance (Nat (O n), Nat (I m)) => CompareN (O n) (I m) where type Compare (O n) (I m) = Join IsLesser  (Compare n m)
instance (Nat (I n), Nat (O m)) => CompareN (I n) (O m) where type Compare (I n) (O m) = Join IsGreater (Compare n m)
instance (Nat (I n), Nat (I m)) => CompareN (I n) (I m) where type Compare (I n) (I m) = Compare n m

----------------------------------------------------------------
-- Positive and Non-zero numbers

instance Nat (I n) => Positive (I n)
instance Nat (O n) => Positive (O n)

instance Nat (I n) => NonZero (I n)
instance Nat (O n) => NonZero (O n)

----------------------------------------------------------------
-- Addition
data Carry      -- Designate carry bit
data NoCarry    -- No carry bit in addition

-- Type family which actually implement addtition of natural numbers
type family Add' n m c :: *

-- Recursion termination without carry bit. Full enumeration is
-- required to avoid overlapping instances
type instance Add'    Z     Z  NoCarry = Z
type instance Add' (O n)    Z  NoCarry = O n
type instance Add' (I n)    Z  NoCarry = I n
type instance Add'    Z  (O n) NoCarry = O n
type instance Add'    Z  (I n) NoCarry = I n
-- Recursion termination with carry bit
type instance Add'    Z   Z      Carry = I Z
type instance Add' (O n)  Z      Carry = I n
type instance Add' (I n)  Z      Carry = Add' (I n) (I Z) NoCarry
type instance Add'    Z  (O n)   Carry = I n
type instance Add'    Z  (I n)   Carry = Add' (I n) (I Z) NoCarry
-- Generic recursion (No carry)
type instance Add' (O n) (O m) NoCarry = O (Add' n m NoCarry)
type instance Add' (I n) (O m) NoCarry = I (Add' n m NoCarry)
type instance Add' (O n) (I m) NoCarry = I (Add' n m NoCarry)
type instance Add' (I n) (I m) NoCarry = O (Add' n m   Carry)
-- Generic recursion (with carry)
type instance Add' (O n) (O m)   Carry = I (Add' n m NoCarry)
type instance Add' (I n) (O m)   Carry = O (Add' n m   Carry)
type instance Add' (O n) (I m)   Carry = O (Add' n m   Carry)
type instance Add' (I n) (I m)   Carry = I (Add' n m   Carry)

-- Enumeration of all possible instances heads is required to avoid
-- overlapping.
type instance Add (O n) (O m) = Normalized (Add' (O n) (O m) NoCarry)
type instance Add (I n) (O m) = Normalized (Add' (I n) (O m) NoCarry)
type instance Add (O n) (I m) = Normalized (Add' (O n) (I m) NoCarry)
type instance Add (I n) (I m) = Normalized (Add' (I n) (I m) NoCarry)
type instance Add (O n)    Z  = Normalized (Add' (O n)    Z  NoCarry)
type instance Add (I n)    Z  = Normalized (Add' (I n)    Z  NoCarry)
type instance Add    Z  (O n) = Normalized (Add'    Z  (O n) NoCarry)
type instance Add    Z  (I n) = Normalized (Add'    Z  (I n) NoCarry)
type instance Add    Z     Z  = Normalized (Add'    Z     Z  NoCarry)

----------------------------------------------------------------
-- Subtraction
data Borrow     -- Designate carry bit
data NoBorrow   -- No carry bit in addition

-- Type class which actually implement addtition of natural numbers
type family Sub' n m c :: *

-- Recursion termination without carry bit. Full enumeration is
-- required to avoid overlapping instances
type instance Sub'    Z     Z  NoBorrow = Z
type instance Sub' (O n)    Z  NoBorrow = O n
type instance Sub' (I n)    Z  NoBorrow = I n
-- Recursion termination with carry bit
type instance Sub' (O n)  Z      Borrow = I (Sub' n Z Borrow)
type instance Sub' (I n)  Z      Borrow = O n
-- Generic recursion (No carry)
type instance Sub' (O n) (O m) NoBorrow = O (Sub' n m NoBorrow)
type instance Sub' (I n) (O m) NoBorrow = I (Sub' n m NoBorrow)
type instance Sub' (O n) (I m) NoBorrow = I (Sub' n m   Borrow)
type instance Sub' (I n) (I m) NoBorrow = O (Sub' n m NoBorrow)
-- -- Generic recursion (with carry)
type instance Sub' (O n) (O m)   Borrow = I (Sub' n m   Borrow)
type instance Sub' (I n) (O m)   Borrow = O (Sub' n m NoBorrow)
type instance Sub' (O n) (I m)   Borrow = O (Sub' n m   Borrow)
type instance Sub' (I n) (I m)   Borrow = I (Sub' n m   Borrow)

-- Enumeration of all possible instances heads is required to avoid
-- overlapping.
type instance Sub (O n) (O m) = Normalized (Sub' (O n) (O m) NoBorrow)
type instance Sub (I n) (O m) = Normalized (Sub' (I n) (O m) NoBorrow)
type instance Sub (O n) (I m) = Normalized (Sub' (O n) (I m) NoBorrow)
type instance Sub (I n) (I m) = Normalized (Sub' (I n) (I m) NoBorrow)
type instance Sub (O n)    Z  = Normalized (Sub' (O n)    Z  NoBorrow)
type instance Sub (I n)    Z  = Normalized (Sub' (I n)    Z  NoBorrow)
type instance Sub    Z     Z  = Normalized (Sub'    Z     Z  NoBorrow)

----------------------------------------------------------------
-- Multiplication
----------------------------------------------------------------

type instance Mul n    Z  = Z
type instance Mul n (O m) = Normalized (O (Mul n m))
type instance Mul n (I m) = Normalized (Add' n (O (Mul n m)) NoCarry)
