{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Runtime.Client.Data (
    ContentType
  , RpcClientCodec (..)
  ) where


import           Corba.Runtime.Core.Data

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (Either)


type ContentType = ByteString

data RpcClientCodec a = RpcClientCodec {
    codecContentType :: ContentType
  , codecRequest :: RpcRequest a -> BSL.ByteString
  , codecResponse :: BSL.ByteString -> Either ErrorMessage (RpcResponse a)
  }
