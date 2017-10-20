{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Corba.Runtime.Wai (
    Route
  , RpcHandler
  , middleware
  ) where


import           Control.Applicative (pure)
import           Control.Monad (Monad (..))

import           Corba.Runtime.Core.Data
import           Corba.Runtime.Wai.Data

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (Either (..))
import           Data.Eq (Eq (..))
import           Data.Function ((.), ($), flip)
import           Data.Functor (fmap)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (Maybe (..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

import           System.IO (IO)


middleware :: Route -> [RpcHandler] -> Wai.Middleware
middleware route handlers next req resp =
  if Wai.pathInfo req == route
    then
      serveRequest handlers req >>= resp
    else
      next req resp

serveRequest :: [RpcHandler] -> Wai.Request -> IO Wai.Response
serveRequest handlers req =
  let
    handler = buildRpcHandler handlers
  in
    case Wai.requestMethod req of
      "POST" ->
        case header HTTP.hContentType req of
          Just ctype ->
            case handler ctype of
              Just fun -> do
                -- This can use unbounded memory, be careful when exposing to the public
                bs <- Wai.strictRequestBody req
                case fun bs of
                  Right doit -> do
                    -- RPC response ready to send
                    (code, body) <- doit
                    pure $ respond code ctype body
                  Left err ->
                    -- Failed to parse request
                    pure $ plain HTTP.status400 err
              Nothing ->
                -- Unsupported Content-Type
                pure error415
          Nothing ->
            -- No Content-Type header
            pure error415
      _ ->
        -- Wasn't a POST
        pure error405

buildRpcHandler :: [RpcHandler] -> (ContentType -> Maybe (BSL.ByteString -> Either Text (IO (HTTP.Status, BSL.ByteString))))
buildRpcHandler handlers =
  flip M.lookup . M.fromList . flip fmap handlers $ \handler ->
    (handleContentType handler, handleRpc handler)

handleRpc :: RpcHandler -> (BSL.ByteString -> Either Text (IO (HTTP.Status, BSL.ByteString)))
handleRpc (RpcHandler _mime method request response) breq =
  flip fmap (request breq) $ \req ->
    fmap (\rsp -> (responseCode rsp, response rsp)) (method req)

-- These are important for visibility in AWS metrics
responseCode :: RpcResponse a -> HTTP.Status
responseCode rsp =
  case rsp of
    RpcResponseOk _ ->
      HTTP.status200
    RpcError _ ->
      HTTP.status500
    RpcMethodMissing _ ->
      HTTP.status404

-- -----------------------------------------------------------------------------

header :: HTTP.HeaderName -> Wai.Request -> Maybe ByteString
header hn req =
  L.lookup hn (Wai.requestHeaders req)

error405 :: Wai.Response
error405 =
  plain HTTP.status405 "405 Method Not Allowed"

error415 :: Wai.Response
error415 =
  plain HTTP.status415 "415 Unsupported Media Type"

pattern PlainContentType :: ByteString
pattern PlainContentType = "text/plain; charset=utf-8"

respond :: HTTP.Status -> ContentType -> BSL.ByteString -> Wai.Response
respond status ctype =
  Wai.responseLBS status [(HTTP.hContentType, ctype)]

plain :: HTTP.Status -> Text -> Wai.Response
plain status =
    respond status PlainContentType
  . BSL.fromStrict
  . TE.encodeUtf8
