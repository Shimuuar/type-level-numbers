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

n0 :: N0; n0 = undefined
n1 :: N1; n1 = undefined
n2 :: N2; n2 = undefined
n3 :: N3; n3 = undefined
n4 :: N4; n4 = undefined
n5 :: N5; n5 = undefined
n6 :: N6; n6 = undefined
n7 :: N7; n7 = undefined
n8 :: N8; n8 = undefined
n9 :: N9; n9 = undefined
