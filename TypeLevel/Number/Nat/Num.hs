{-# LANGUAGE TemplateHaskell #-}
module TypeLevel.Number.Nat.Num where

import TypeLevel.Number.Nat

type N0 = $(natT 0)
type N1 = $(natT 1)
type N2 = $(natT 2)
type N3 = $(natT 3)
type N4 = $(natT 4)
type N5 = $(natT 5)
type N6 = $(natT 6)
type N7 = $(natT 7)
type N8 = $(natT 8)
type N9 = $(natT 9)
