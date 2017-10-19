{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Corba.Runtime.Wai.Json where


import           Control.Applicative (pure)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import           Corba.Runtime.Core.Data
import qualified Corba.Runtime.Core.Json as Json
import           Corba.Runtime.Wai.Data

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (either)
import           Data.Function ((.), ($), flip)
import           Data.Functor (fmap)
import qualified Data.Map.Strict as M
import           Data.Maybe (Maybe (..), maybe)
import           Data.Monoid ((<>))

import           System.IO (IO)

-- import           Prelude


data JsonMethod = forall a b. JsonMethod {
    jsonMethodName :: MethodName
  , jsonMethodDecode :: Aeson.Value -> Aeson.Parser a
  , jsonMethodRun :: a -> ExceptT ErrorMessage IO b
  , jsonMethodEncode :: b -> Aeson.Value
  }

runJsonMethod :: JsonMethod -> Aeson.Value -> ExceptT ErrorMessage IO Aeson.Value
runJsonMethod (JsonMethod n decode run encode) v = do
  req <- maybe (throwE (decodeErrorMessage n)) pure (Aeson.parseMaybe decode v)
  rsp <- run req
  pure (encode rsp)

jsonV1 :: [JsonMethod] -> RpcHandler
jsonV1 methods =
  RpcHandler {
      handleContentType = Json.CorbaJsonV1MimeType
    , handleMethod = rpcDispatchV1 methods
    , handleRequest = decodeRpcRequestV1
    , handleResponse = encodeRpcResponseV1
    }

rpcDispatchV1 :: [JsonMethod] -> (RpcRequest Aeson.Value -> IO (RpcResponse Aeson.Value))
rpcDispatchV1 methods req =
  let
    dispatchMap :: M.Map MethodName (Aeson.Value -> ExceptT ErrorMessage IO Aeson.Value)
    dispatchMap =
      M.fromList . flip fmap methods $ \method ->
        (jsonMethodName method, runJsonMethod method)
    methodName =
      rpcRequestMethod req
  in
    case M.lookup methodName dispatchMap of
      Just method -> do
        eval <- runExceptT (method (rpcRequestPayload req))
        pure $ either RpcError RpcResponseOk eval
      Nothing ->
        pure (RpcMethodMissing methodName)

encodeRpcResponseV1 :: RpcResponse Aeson.Value -> BSL.ByteString
encodeRpcResponseV1 rsp =
  Aeson.encode (Json.rpcResponseToJson rsp)

decodeRpcRequestV1 :: BSL.ByteString -> Maybe (RpcRequest Aeson.Value)
decodeRpcRequestV1 bs = do
  value <- Aeson.decode' bs
  Aeson.parseMaybe Json.rpcRequestFromJson value

decodeErrorMessage :: MethodName -> ErrorMessage
decodeErrorMessage (MethodName mn) =
  ErrorMessage $
    "JSON_V1: Failed to decode request for method '" <> mn <> "'"
