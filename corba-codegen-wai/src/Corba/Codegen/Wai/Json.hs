{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Codegen.Wai.Json (
    generateWaiModuleV1
  , generateWaiV1
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
helloJsonV1 :: HelloService (ExceptT ErrorMessage IO) -> RpcHandler
helloJsonV1 svc =
  jsonV1 [
      JsonMethod {
          jsonMethodName = MethodName "hello"
        , jsonMethodDecode = Aeson.generateFromJsonNameV1 (M.Name "HelloRequest")
        , jsonMethodRun = hello svc
        , jsonMethodEncode = Aeson.generateToJsonNameV1 (M.Name "HelloResponse")
        }
    ]
--}

generateWaiModuleV1 :: CorbaResult -> Text
generateWaiModuleV1 cr =
  T.unlines [
      "import           Control.Monad.Trans.Except (ExceptT)"
    , "import           Corba.Runtime.Wai"
    , "import           Corba.Runtime.Wai.Json"
    , "import           Corba.Runtime.Core.Data"
    , T.unlines (fmap (T.pack . TH.pprint) (generateWaiV1 cr))
    ]

generateWaiV1 :: CorbaResult -> [Dec]
generateWaiV1 (CorbaResult s@(Service name _methods) _data) = [
    XTH.sig (handlerName name) (XTH.arrowT_ (serviceType name) (XTH.conT (TH.mkName "RpcHandler")))
  , generateRpcHandlerV1 s
  ]

generateRpcHandlerV1 :: Service -> Dec
generateRpcHandlerV1 (Service name methods) =
  XTH.val_ (XTH.varP (handlerName name)) $
    XTH.lamE [XTH.varP serviceBound] $
      XTH.appE (XTH.varE (TH.mkName "jsonV1")) $
        XTH.listE $
          fmap generateJsonMethodV1 methods

generateJsonMethodV1 :: Method -> Exp
generateJsonMethodV1 (Method (MethodName name) (TypeName req) (TypeName rsp)) =
  XTH.recConE (TH.mkName "JsonMethod") [
      (TH.mkName "jsonMethodName", XTH.appE (XTH.conE (TH.mkName "MethodName")) (XTH.litE (XTH.stringL_ name)))
    , (TH.mkName "jsonMethodDecode", XTH.varE (Aeson.generateFromJsonNameV1 (M.Name req)))
    , (TH.mkName "jsonMethodRun", XTH.appE (XTH.varE (XTH.mkName_ name)) (XTH.varE serviceBound))
    , (TH.mkName "jsonMethodEncode", XTH.varE (Aeson.generateToJsonNameV1 (M.Name rsp)))
    ]

handlerName :: ServiceName -> TH.Name
handlerName (ServiceName n) =
  XTH.mkName_ $
    n <> "JsonV1"

serviceType :: ServiceName -> TH.Type
serviceType sn =
  XTH.appT
    (XTH.conT (Data.generateServiceNameV1 sn))
    serviceMonad

serviceBound :: TH.Name
serviceBound =
  TH.mkName "svc"

serviceMonad :: TH.Type
serviceMonad =
  XTH.appT
    (XTH.appT
      (XTH.conT (TH.mkName "ExceptT"))
      (XTH.conT (TH.mkName "ErrorMessage")))
    (XTH.conT (TH.mkName "IO"))
