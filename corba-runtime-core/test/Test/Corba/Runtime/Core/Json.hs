{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Corba.Runtime.Core.Json (
    tests
  ) where


import qualified Corba.Runtime.Core.Json as Json

import qualified Data.Aeson.Types as Aeson

import           Hedgehog

import           Prelude

import           Test.Corba.Runtime.Core.Gen


prop_rpcrequest_tripping :: Property
prop_rpcrequest_tripping =
  property $ do
    req <- forAll genRpcRequest
    trippingJson req Json.rpcRequestToJson Json.rpcRequestFromJson

prop_rpcresponse_tripping :: Property
prop_rpcresponse_tripping =
  property $ do
    rsp <- forAll genRpcResponse
    trippingJson rsp Json.rpcResponseToJson Json.rpcResponseFromJson

trippingJson :: MonadTest m => Show b => Show a => Eq a => a -> (a -> b) -> (b -> Aeson.Parser a) -> m ()
trippingJson a to from =
  tripping a to (Aeson.parseEither from)

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
