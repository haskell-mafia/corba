{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Codegen.Client.Json (
    generateClientModuleV1
  , generateClientV1
  ) where


import qualified Corba.Codegen.Aeson as Aeson
import qualified Corba.Codegen.Data as Data
import           Corba.Core (CorbaResult (..))
import           Corba.Core.Data.Service

import qualified Data.Text as T

import           Language.Haskell.TH (Dec, Exp)
import qualified Language.Haskell.TH as TH

import qualified Machinator.Core.Data.Definition as M

import           P

import qualified X.Language.Haskell.TH.Syntax as XTH


{--

import           Corba.Runtime.Client (ClientError, RequestModifier)
import           Corba.Runtime.Client.Json

helloClientJsonV1 :: Monad m => RequestModifier m -> HelloService (ExceptT ClientError m)
helloClientJsonV1 mgr =
  HelloService {
      hello = jsonRequest mgr (MethodName "hello") (Aeson.generateToJsonNameV1 req) (Aeson.generateFromJsonNameV1 rsp)
    }

--}


generateClientModuleV1 :: CorbaResult -> Text
generateClientModuleV1 cr =
  T.unlines [
      "import           Control.Monad (Monad)"
    , "import           Control.Monad.Trans.Except (ExceptT)"
    , "import           Corba.Runtime.Wai"
    , "import           Corba.Runtime.Wai.Json"
    , "import           Corba.Runtime.Core.Data"
    , T.unlines (fmap (T.pack . TH.pprint) (generateClientV1 cr))
    ]

generateClientV1 :: CorbaResult -> [Dec]
generateClientV1 (CorbaResult s@(Service name _methods) _data) = [
    XTH.sig (clientName name) (clientType name)
  , generateClientRecordV1 s
  ]

generateClientRecordV1 :: Service -> Dec
generateClientRecordV1 (Service name methods) =
  XTH.val_ (XTH.varP (clientName name)) $
    XTH.lamE [XTH.varP clientBound] $
      XTH.recConE (Data.generateServiceNameV1 name) $
        with methods $ \(Method mn@(MethodName mname) req rsp) ->
          (XTH.mkName_ mname, jsonRequest mn req rsp)

jsonRequest :: MethodName -> TypeName -> TypeName -> Exp
jsonRequest (MethodName mn) (TypeName req) (TypeName rsp) =
  XTH.applyE (XTH.varE (TH.mkName "jsonRequest")) [
      XTH.varE clientBound
    , XTH.appE (XTH.conE (TH.mkName "MethodName")) (XTH.litE (XTH.stringL_ mn))
    , XTH.varE (Aeson.generateToJsonNameV1 (M.Name req))
    , XTH.varE (Aeson.generateFromJsonNameV1 (M.Name rsp))
    ]

clientName :: ServiceName -> TH.Name
clientName (ServiceName n) =
  XTH.mkName_ $
    n <> "ClientJsonV1"

clientType :: ServiceName -> TH.Type
clientType sn =
  TH.ForallT [TH.PlainTV var] [XTH.appT (XTH.conT (TH.mkName "Monad")) (TH.VarT var)] $
    XTH.arrowT_
      (XTH.appT (XTH.conT (TH.mkName "RequestModifier")) (TH.VarT var))
      (XTH.appT (XTH.conT (Data.generateServiceNameV1 sn)) clientMonad)
  where
    var = TH.mkName "m"

clientMonad :: TH.Type
clientMonad =
  XTH.appT
    (XTH.appT
      (XTH.conT (TH.mkName "ExceptT"))
      (XTH.conT (TH.mkName "ClientError")))
    (TH.VarT (TH.mkName "m"))

clientBound :: TH.Name
clientBound =
  TH.mkName "mgr"
