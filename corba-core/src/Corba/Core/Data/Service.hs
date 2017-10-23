{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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


import           Data.Data (Data, Typeable)

import           GHC.Generics (Generic)

import           P


newtype ServiceName = ServiceName Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data Service = Service {
    serviceName :: ServiceName
  , serviceMethods :: [Method]
  } deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data Method = Method {
    methodName :: MethodName
  , methodRequest :: TypeName
  , methodResponse :: TypeName
  } deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

newtype MethodName = MethodName Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

newtype TypeName = TypeName Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
