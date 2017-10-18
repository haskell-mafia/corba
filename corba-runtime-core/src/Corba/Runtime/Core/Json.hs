{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Corba.Runtime.Core.Json (
    rpcRequestToJson
  , rpcRequestFromJson
  , rpcResponseToJson
  , rpcResponseFromJson
  , pattern CorbaJsonV1MimeType
  ) where


import           Corba.Runtime.Core.Data

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T

import           Prelude


rpcRequestToJson :: RpcRequest Aeson.Value -> Aeson.Value
rpcRequestToJson (RpcRequest (MethodName method) payload) =
  Aeson.object [
      "method" .= method
    , "payload" .= payload
    ]

rpcRequestFromJson :: Aeson.Value -> Aeson.Parser (RpcRequest Aeson.Value)
rpcRequestFromJson v =
  case v of
    Aeson.Object o ->
      RpcRequest
        <$> fmap MethodName (o .: "method")
        <*> o .: "payload"
    x ->
      Aeson.typeMismatch "RpcRequest" x

rpcResponseToJson :: RpcResponse Aeson.Value -> Aeson.Value
rpcResponseToJson response =
  case response of
    RpcResponseOk payload ->
      Aeson.object [
          "response" .= ("ok" :: Text)
        , "payload" .= payload
        ]
    RpcError (ErrorMessage msg) ->
      Aeson.object [
          "response" .= ("error" :: Text)
        , "error" .= msg
        ]
    RpcMethodMissing (MethodName method) ->
      Aeson.object [
          "response" .= ("method-missing" :: Text)
        , "method" .= method
        ]

rpcResponseFromJson :: Aeson.Value -> Aeson.Parser (RpcResponse Aeson.Value)
rpcResponseFromJson v =
  case v of
    Aeson.Object o -> do
      response <- o .: "response" :: Aeson.Parser Text
      case response of
        "ok" ->
          RpcResponseOk
            <$> o .: "payload"
        "error" ->
          RpcError
            <$> fmap ErrorMessage (o .: "error")
        "method-missing" ->
          RpcMethodMissing
            <$> fmap MethodName (o .: "method")
        _ ->
          fail $ "RpcResponse.response: unexpected value '" ++ T.unpack response ++ "'"
    x ->
      Aeson.typeMismatch "RpcResponse" x


pattern CorbaJsonV1MimeType :: ByteString
pattern CorbaJsonV1MimeType = "application/vnd.ambiata.corba.v1+json"
