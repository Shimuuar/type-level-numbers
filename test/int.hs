{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Language.Haskell.TH

import TestNat


main :: IO ()
main = do
  putStrLn $ unlines $(listE (testAddZ <$> [-9..9] <*> [-9..9]))
  putStrLn $ unlines $(listE (testSubZ <$> [-9..9] <*> [-9..9]))
  putStrLn $ unlines $(listE (testMulZ <$> [-9..9] <*> [-9..9]))
