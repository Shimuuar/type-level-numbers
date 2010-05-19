{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Language.Haskell.TH

import TestNat


main :: IO ()
main = do
  putStrLn $ unlines $(listE (testAddZ <$> [-27..27] <*> [-27..27]))
  putStrLn $ unlines $(listE (testSubZ <$> [-27..27] <*> [-27..27]))
  putStrLn $ unlines $(listE (testMulZ <$> [-27..27] <*> [-27..27]))
