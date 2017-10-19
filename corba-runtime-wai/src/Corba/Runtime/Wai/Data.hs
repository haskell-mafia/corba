{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Corba.Runtime.Wai.Data (
    Route
  , ContentType
  , RpcHandler (..)
  ) where


import           Corba.Runtime.Core.Data

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (Maybe)
import           Data.Text (Text)

import           System.IO (IO)


type Route = [Text]

type ContentType = ByteString

data RpcHandler = forall a. RpcHandler {
    handleContentType :: ContentType
  , handleMethod :: RpcRequest a -> IO (RpcResponse a)
  , handleRequest :: BSL.ByteString -> Maybe (RpcRequest a)
  , handleResponse :: RpcResponse a -> BSL.ByteString
  }
