{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Codegen.Data (
    generateDataModuleV1
  , generateDataV1
  , generateDefinitionV1
  ) where


import           Corba.Core
import           Corba.Core.Data.Service

import qualified Data.Char as Char
import qualified Data.Text as T

import           Machinator.Core (Definition (..))
import           Machinator.Core.Data.Definition (DataType (..), Name (..), Type (..), Ground (..))

import           P

import qualified Language.Haskell.TH as TH
import qualified X.Language.Haskell.TH.Syntax as XTH


generateDataModuleV1 :: CorbaResult -> T.Text
generateDataModuleV1 cr =
  T.unlines [
      "import           Data.Bool (Bool (..))"
    , "import           Data.Text (Text)"
    , T.unlines (fmap (T.pack . TH.pprint) (generateDataV1 cr))
    ]

generateDataV1 :: CorbaResult -> [TH.Dec]
generateDataV1 (CorbaResult svc defs) =
  fold [
      [generateServiceV1 svc]
    , fmap generateDefinitionV1 defs
    ]

-- | Generateerate the service data type
generateServiceV1 :: Service -> TH.Dec
generateServiceV1 (Service (ServiceName n) ms) =
  XTH.data_ (XTH.mkName_ (T.toTitle n <> "Service")) [XTH.mkName_ "m"] . pure . XTH.recC_ (XTH.mkName_ (T.toTitle n <> "Service")) $
    with ms $ \(Method (MethodName mn) (TypeName req) (TypeName res)) ->
      (,) (XTH.mkName_ mn) (XTH.arrowT_
          (XTH.conT (XTH.mkName_ req))
          (XTH.appT (TH.VarT . XTH.mkName_ $ "m") (XTH.conT (XTH.mkName_ res)))
        )

-- | Generateerate a TH type declaration from a Machinator 'Definition'.
generateDefinitionV1 :: Definition -> TH.Dec
generateDefinitionV1 (Definition nn@(Name n) d) =
  case d of
    Variant nts ->
      XTH.data_ (XTH.mkName_ n) [] (fmap (uncurry generateConV1) (toList nts))
    Record fts ->
      XTH.data_ (XTH.mkName_ n) [] [generateRecV1 nn fts]

-- | Generateerate a regular variant constructor.
generateConV1 :: Name -> [Type] -> TH.Con
generateConV1 (Name n) ts =
  XTH.normalC_' (XTH.mkName_ n) (fmap generateTypeV1 ts)

-- | Generateerate a record constructor.
generateRecV1 :: Name -> [(Name, Type)] -> TH.Con
generateRecV1 nn@(Name n) fts =
  XTH.recC_' (XTH.mkName_ n) (fmap (bimap (generateRecFieldNameV1 nn) generateTypeV1) fts)

-- | The heuristic used to derive Haskell record field names.
--
-- For a record named 'FooBar' with a field 'bazQuux', the Haskell
-- field will be named 'fooBarBazQuux'.
generateRecFieldNameV1 :: Name -> Name -> TH.Name
generateRecFieldNameV1 (Name tn) (Name fn) =
  XTH.mkName_ (T.map Char.toLower (T.take 1 tn) <> T.drop 1 tn <> T.toTitle fn)

-- | Generate a regular type from a Machinator 'Type'.
generateTypeV1 :: Type -> TH.Type
generateTypeV1 ty =
  case ty of
    Variable (Name tn) ->
      XTH.conT (XTH.mkName_ tn)
    GroundT g ->
      case g of
        StringT ->
          XTH.conT (XTH.mkName_ "Text")
        BoolT ->
          XTH.conT (XTH.mkName_ "Bool")
    ListT t2 ->
      XTH.listT_ (generateTypeV1 t2)
