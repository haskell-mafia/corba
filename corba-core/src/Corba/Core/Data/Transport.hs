{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Core.Data.Transport (
    Transport (..)
  ) where


import           P


data Transport =
    Json
  deriving (Eq, Ord, Show)
