{-# LANGUAGE EmptyDataDecls #-}
module Types.Number.Nat.Types ( I
                              , O
                              , Z
                              ) where

-- | One bit.
data I n
-- | Zero bit.
data O n
-- | Bit stream terminator.
data Z
