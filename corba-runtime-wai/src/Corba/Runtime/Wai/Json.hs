{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Corba.Runtime.Wai.Json (
    JsonMethod (..)
  , jsonV1
  ) where


import           Control.Applicative (pure)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import           Corba.Runtime.Core.Data
import qualified Corba.Runtime.Core.Json as Json
import           Corba.Runtime.Wai.Data

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (Char)
import           Data.Either (Either (..), either)
import           Data.Function ((.), ($), flip)
import           Data.Functor (fmap)
import qualified Data.Map.Strict as M
import           Data.Maybe (Maybe (..), maybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           System.IO (IO)


data JsonMethod = forall a b. JsonMethod {
    jsonMethodName :: MethodName
  , jsonMethodDecode :: Aeson.Value -> Aeson.Parser a
  , jsonMethodRun :: a -> ExceptT ErrorMessage IO b
  , jsonMethodEncode :: b -> Aeson.Value
  }

runJsonMethod :: JsonMethod -> Aeson.Value -> ExceptT ErrorMessage IO Aeson.Value
runJsonMethod (JsonMethod n decode run encode) v = do
  req <- either (throwE . decodeErrorMessage n) pure (Aeson.parseEither decode v)
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

decodeRpcRequestV1 :: BSL.ByteString -> Either Text (RpcRequest Aeson.Value)
decodeRpcRequestV1 bs = do
  value <- maybe (Left "Invalid JSON") Right (Aeson.decode' bs)
  first T.pack (Aeson.parseEither Json.rpcRequestFromJson value)

decodeErrorMessage :: MethodName -> [Char] -> ErrorMessage
decodeErrorMessage (MethodName mn) err =
  ErrorMessage $
    T.unlines [
        "JSON_V1: Failed to decode request payload for method '" <> mn <> "':"
      , "  " <> T.pack err
      ]
