{-# LANGUAGE TemplateHaskell #-}
module TestNat where
       
import Language.Haskell.TH
import Text.Printf

import TypeLevel.Number.Nat as N
import TypeLevel.Number.Int as I

text :: Bool -> String
text True  = "OK"
text False = "Failed"

----------------------------------------------------------------
-- Natural numbers

testAdd :: Integer -> Integer -> ExpQ
testAdd n m = 
  [| let flag = (n+m) == (N.toInt $ addN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in printf "Add %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testSub :: Integer -> Integer -> ExpQ
testSub n m = 
  [| let flag = (n-m) == (N.toInt $ subN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in printf "Sub %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testMul :: Integer -> Integer -> ExpQ
testMul n m = 
  [| let flag = (n*m) == (N.toInt $ mulN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in printf "Mul %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

----------------------------------------------------------------
-- Integer numbers

testAddZ :: Integer -> Integer -> ExpQ
testAddZ n m = 
  [| let flag = (n+m) == (I.toInt $ addN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in printf "Add %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testSubZ :: Integer -> Integer -> ExpQ
testSubZ n m = 
  [| let flag = (n-m) == (I.toInt $ subN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in printf "Sub %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testMulZ :: Integer -> Integer -> ExpQ
testMulZ n m = 
  [| let flag = (n*m) == (I.toInt $ mulN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in printf "Mul %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

