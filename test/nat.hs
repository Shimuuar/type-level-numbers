{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Language.Haskell.TH

import TestNat


main :: IO ()
main = do
  putStrLn $ unlines $(listE (testAdd <$> [0..8] <*> [0..8]))
  putStrLn $ unlines $(listE [testSub n m | m <- [0..8], n <- [m..8]])
  putStrLn $ unlines $(listE (testMul <$> [0..8] <*> [0..8]))
