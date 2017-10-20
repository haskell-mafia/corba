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
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (Either (..))
import           Data.Function ((.))
import           Data.Functor (fmap)
import           Data.Maybe (maybe)
import qualified Data.Text as T


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

decodeRpcResponseV1 :: BSL.ByteString -> Either ErrorMessage (RpcResponse Aeson.Value)
decodeRpcResponseV1 bs = do
  value <- maybe (Left messageError) pure (Aeson.decode' bs)
  first (ErrorMessage . T.pack) (Aeson.parseEither Json.rpcResponseFromJson value)

messageError :: ErrorMessage
messageError =
  ErrorMessage
    "Invalid JSON"

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
