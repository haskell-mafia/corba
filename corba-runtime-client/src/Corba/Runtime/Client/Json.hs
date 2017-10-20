{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Runtime.Client.Json (
    jsonRequest
  ) where


import           Control.Applicative (pure)
import           Control.Monad ((>>=), Monad)
import           Control.Monad.Trans.Except (ExceptT (..))

import           Corba.Runtime.Client
import           Corba.Runtime.Client.Data
import           Corba.Runtime.Core.Data
import qualified Corba.Runtime.Core.Json as Json

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import           Data.Bifunctor (first)
import           Data.Function ((.))
import           Data.Functor (fmap)
import           Data.Maybe (Maybe)


jsonCodecV1 :: RpcClientCodec Aeson.Value
jsonCodecV1 =
  RpcClientCodec {
      codecContentType = Json.CorbaJsonV1MimeType
    , codecRequest = encodeRpcRequestV1
    , codecResponse = decodeRpcResponseV1
    }

encodeRpcRequestV1 :: RpcRequest Aeson.Value -> BSL.ByteString
encodeRpcRequestV1 req =
  Aeson.encode (Json.rpcRequestToJson req)

decodeRpcResponseV1 :: BSL.ByteString -> Maybe (RpcResponse Aeson.Value)
decodeRpcResponseV1 bs = do
  value <- Aeson.decode' bs
  Aeson.parseMaybe Json.rpcResponseFromJson value

jsonRequest ::
     Monad m
  => RequestModifier m
  -> MethodName
  -> (a -> Aeson.Value)
  -> (Aeson.Value -> Aeson.Parser b)
  -> a
  -> ExceptT ClientError m b
jsonRequest mgr method to from a = do
  send mgr jsonCodecV1 (fmap to (RpcRequest method a))
    >>= ExceptT . pure  . first (ClientSerialisationError . T.pack) . Aeson.parseEither from
