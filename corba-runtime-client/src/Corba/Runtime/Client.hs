{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Runtime.Client (
    ClientError (..)
  , RequestModifier (..)
  , send
  ) where


import           Control.Applicative (pure)
import           Control.Monad (Monad, (=<<))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)

import           Corba.Runtime.Client.Data
import           Corba.Runtime.Core.Data

import qualified Data.ByteString.Lazy as BSL
import           Data.Function (($))
import           Data.Int (Int)
import           Data.Maybe (maybe)
import           Data.Text (Text)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP


data ClientError =
    ClientUserError ErrorMessage
  | ClientMethodMissing MethodName
  | ClientMessageDecodeError
  | ClientUnexpectedStatusCode Int
  | ClientSerialisationError Text

newtype RequestModifier m =
  RequestModifier {
      unRequestModifier :: HTTP.Request -> m (HTTP.Response BSL.ByteString)
    }

send :: Monad m => RequestModifier m -> RpcClientCodec a -> RpcRequest a -> ExceptT ClientError m a
send mgr codec req = do
  resp <- lift $ unRequestModifier mgr HTTP.defaultRequest {
      HTTP.method = "POST"
    , HTTP.requestBody = HTTP.RequestBodyLBS (codecRequest codec req)
    , HTTP.requestHeaders = [(HTTP.hContentType, codecContentType codec)]
    }
  let
    rpcResponse =
      maybe (throwE ClientMessageDecodeError) pure $ codecResponse codec (HTTP.responseBody resp)
    hoistResponse r =
      case r of
        RpcResponseOk a ->
          pure a
        RpcError e ->
          throwE (ClientUserError e)
        RpcMethodMissing m ->
          throwE (ClientMethodMissing m)
  case HTTP.statusCode (HTTP.responseStatus resp) of
    200 ->
      hoistResponse =<< rpcResponse
    404 ->
      hoistResponse =<< rpcResponse
    500 ->
      hoistResponse =<< rpcResponse
    x ->
      throwE (ClientUnexpectedStatusCode x)
