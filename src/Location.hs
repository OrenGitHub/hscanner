{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Location

where

import Data.Aeson
import GHC.Generics

data Location
   = Location
     {
         filename  :: FilePath,
         lineStart :: Int,
         colStart  :: Int,
         colEnd    :: Int
     }
     deriving ( Show, Generic, ToJSON )
