{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Inspect.Graph where

import GHC.Generics
import Data.Aeson
--import Data.Maybe
--import Data.ByteString.Char8

--import Data.Aeson
--import Data.ByteString (ByteString)


data Graph = Simple { values :: [Int] }
           deriving (Show, Generic, ToJSON, FromJSON)

class ToJSON a => Graphable a where
  graph :: a -> Graph

instance Graphable [Int] where
  graph = Simple
