{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Corba.Runtime.Core.Gen where


import           Corba.Runtime.Core.Data

import qualified Data.Aeson as Aeson
import           Data.Functor
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Vector as V

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Prelude


genRpcRequest :: MonadGen m => m (RpcRequest Aeson.Value)
genRpcRequest =
  RpcRequest
    <$> genMethodName
    <*> genArbitraryJson

genRpcResponse :: MonadGen m => m (RpcResponse Aeson.Value)
genRpcResponse =
  Gen.choice [
      RpcResponseOk <$> genArbitraryJson
    , RpcError <$> genErrorMessage
    , RpcMethodMissing <$> genMethodName
    ]

genMethodName :: MonadGen m => m MethodName
genMethodName =
  MethodName
    <$> Gen.text (Range.linear 0 100) Gen.alphaNum

genErrorMessage :: MonadGen m => m ErrorMessage
genErrorMessage =
  ErrorMessage
    <$> Gen.text (Range.linear 0 100) Gen.alphaNum

-- -----------------------------------------------------------------------------

genArbitraryJson :: MonadGen m => m Aeson.Value
genArbitraryJson =
  Gen.sized genArbitraryJson'

genArbitraryJson' :: MonadGen m => Range.Size -> m Aeson.Value
genArbitraryJson' size =
  Gen.choice [
      Aeson.Object <$> genArbitraryObject size
    , Aeson.Array <$> genArbitraryArray size
    , Aeson.String <$> genArbitraryString size
    , Aeson.Bool <$> Gen.bool_
    , pure Aeson.Null
    -- FIXME Number is missing
    ]

genArbitraryObject :: MonadGen m => Range.Size -> m Aeson.Object
genArbitraryObject size@(Size range) =
  mapToHashMap
    <$> Gen.map (Range.linear 0 range) ((,) <$> genArbitraryObjectKey <*> genArbitraryJson' (size `div` 2))

genArbitraryObjectKey :: MonadGen m => m Text
genArbitraryObjectKey =
  Gen.text (Range.linear 1 50) Gen.ascii {- Not all that arbitrary - just more readable -}

genArbitraryArray :: MonadGen m => Range.Size -> m (V.Vector Aeson.Value)
genArbitraryArray size@(Size range) =
  V.fromList
    <$> Gen.list (Range.linear 1 range) (genArbitraryJson' (size `div` 2))

genArbitraryString :: MonadGen m => Range.Size -> m Text
genArbitraryString (Size range) =
  Gen.text (Range.linear 0 range) Gen.unicode

mapToHashMap :: Ord k => Hashable k => M.Map k v -> HM.HashMap k v
mapToHashMap =
  HM.fromList . M.toList
