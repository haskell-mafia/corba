{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Core.Data.Service (
    ServiceName (..)
  , Service (..)
  , Method (..)
  , MethodName (..)
  , TypeName (..)
  ) where


import           P


newtype ServiceName = ServiceName Text
  deriving (Eq, Ord, Read, Show)

data Service = Service {
    serviceName :: ServiceName
  , serviceMethods :: [Method]
  } deriving (Eq, Ord, Read, Show)

data Method = Method {
    methodName :: MethodName
  , methodRequest :: TypeName
  , methodResponse :: TypeName
  } deriving (Eq, Ord, Read, Show)

newtype MethodName = MethodName Text
  deriving (Eq, Ord, Read, Show)

newtype TypeName = TypeName Text
  deriving (Eq, Ord, Read, Show)
