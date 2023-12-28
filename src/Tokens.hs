{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokens

where

import Location
import Data.Aeson
import GHC.Generics

data Token
   = TokInt Int Location
   | TokID String Location
   | TokPlus Location
   deriving ( Show, Generic, ToJSON )

data Tokens
   = Tokens
     {
         filename :: FilePath,
         content :: [ Token ]
     }
     deriving ( Show, Generic, ToJSON )
