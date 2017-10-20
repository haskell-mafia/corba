{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Codegen.Data (
    genResultV1
  , genServiceV1
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


genResultV1 :: CorbaResult -> T.Text
genResultV1 (CorbaResult s ds) =
  T.unlines . fmap (T.pack . TH.pprint) . mconcat $ [
      pure . genServiceV1 $ s
    , fmap genTypesV1 ds
    ]

-- | Generate the service data type
genServiceV1 :: Service -> TH.Dec
genServiceV1 (Service (ServiceName n) ms) =
  XTH.data_ (XTH.mkName_ n) [XTH.mkName_ "m"] . pure . XTH.recC_ (XTH.mkName_ n) $
    with ms $ \(Method (MethodName mn) (TypeName req) (TypeName res)) ->
      (,) (XTH.mkName_ mn) (XTH.arrowT_
          (XTH.conT (XTH.mkName_ req))
          (XTH.appT (TH.VarT . XTH.mkName_ $ "m") (XTH.conT (XTH.mkName_ res)))
        )

-- | Generate a TH type declaration from a Machinator 'Definition'.
genTypesV1 :: Definition -> TH.Dec
genTypesV1 (Definition nn@(Name n) d) =
  case d of
    Variant nts ->
      XTH.data_ (XTH.mkName_ n) [] (fmap (uncurry genConV1) (toList nts))
    Record fts ->
      XTH.data_ (XTH.mkName_ n) [] [genRecV1 nn fts]

-- | Generate a regular variant constructor.
genConV1 :: Name -> [Type] -> TH.Con
genConV1 (Name n) ts =
  XTH.normalC_' (XTH.mkName_ n) (fmap genTypeV1 ts)

-- | Generate a record constructor.
genRecV1 :: Name -> [(Name, Type)] -> TH.Con
genRecV1 nn@(Name n) fts =
  XTH.recC_' (XTH.mkName_ n) (fmap (bimap (genRecFieldNameV1 nn) genTypeV1) fts)

-- | The heuristic used to derive Haskell record field names.
--
-- For a record named 'FooBar' with a field 'bazQuux', the Haskell
-- field will be named 'fooBarBazQuux'.
--
-- This is a decent enough heuristic with few collisions, but should
-- perhaps be configurable by the end-user.
genRecFieldNameV1 :: Name -> Name -> TH.Name
genRecFieldNameV1 (Name tn) (Name fn) =
  XTH.mkName_ (T.map Char.toLower (T.take 1 tn) <> T.drop 1 tn <> T.toTitle fn)

-- | Generate a regular type from a Machinator 'Type'.
genTypeV1 :: Type -> TH.Type
genTypeV1 ty =
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
      XTH.listT_ (genTypeV1 t2)
