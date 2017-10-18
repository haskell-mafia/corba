{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Corba.Runtime.Core.Data (
    RpcRequest (..)
  , RpcResponse (..)
  , MethodName (..)
  , ErrorMessage (..)
  ) where


import           Data.Foldable (Foldable)
import           Data.Functor (Functor)
import           Data.Text (Text)
import           Data.Traversable (Traversable)

import           Prelude (Eq, Ord, Show)


data RpcRequest a = RpcRequest {
    rpcRequestMethod :: MethodName
  , rpcRequestPayload :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data RpcResponse a =
    RpcResponseOk a
  | RpcError ErrorMessage
  | RpcMethodMissing MethodName
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype MethodName = MethodName {
    unMethodName :: Text
  } deriving (Eq, Ord, Show)

newtype ErrorMessage = ErrorMessage {
    unErrorMessage :: Text
  } deriving (Eq, Ord, Show)
