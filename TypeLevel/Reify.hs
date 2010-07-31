{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : TypeLevel.Reify
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
module TypeLevel.Reify ( Witness(..)
                       , Reify(..)
                       ) where


data Witness t a = Witness { getValue :: a }
                   deriving Show

-- | Convert type level into value level using 
class Reify t a where
  witness :: Witness t a

       
       