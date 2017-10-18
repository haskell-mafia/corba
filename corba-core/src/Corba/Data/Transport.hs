{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Data.Transport (
    Transport (..)
  ) where


import           P


data Transport =
    Json
  deriving (Eq, Ord, Show)
