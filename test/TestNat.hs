{-# LANGUAGE TemplateHaskell #-}
module TestNat where
       
import Language.Haskell.TH
import Text.Printf

import Types.Number.Nat
import Types.Number.Int

text :: Bool -> String
text flag = if flag then "OK" else "Failed"

----------------------------------------------------------------
-- Natural numbers

testAdd :: Integer -> Integer -> ExpQ
testAdd n m = 
  [| let flag = (n+m) == (toInt $ addN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in printf "Add %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testSub :: Integer -> Integer -> ExpQ
testSub n m = 
  [| let flag = (n-m) == (toInt $ subN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in printf "Sub %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testMul :: Integer -> Integer -> ExpQ
testMul n m = 
  [| let flag = (n*m) == (toInt $ mulN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in printf "Mul %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

----------------------------------------------------------------
-- Integer numbers

testAddZ :: Integer -> Integer -> ExpQ
testAddZ n m = 
  [| let flag = (n+m) == (toInt $ addN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in printf "Add %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testSubZ :: Integer -> Integer -> ExpQ
testSubZ n m = 
  [| let flag = (n-m) == (toInt $ subN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in printf "Sub %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

testMulZ :: Integer -> Integer -> ExpQ
testMulZ n m = 
  [| let flag = (n*m) == (toInt $ mulN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in printf "Mul %3i %3i : %s" (n::Integer) (m::Integer) (text flag) :: String
   |]

