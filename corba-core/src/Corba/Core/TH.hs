{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Core.TH where


import           Control.Monad.Trans.Except (runExceptT)

import           Corba.Core

import qualified Data.Text as T

import           Language.Haskell.TH (Q, Exp, Dec)
import qualified Language.Haskell.TH as TH

import           P

import           System.IO (FilePath)

import           X.Language.Haskell.TH (dataExp)


loadService :: FilePath -> [FilePath] -> Q Exp
loadService srv mcn =
  loadService' srv mcn dataExp

loadService' :: FilePath -> [FilePath] -> (CorbaResult -> Q a) -> Q a
loadService' srv mcn f = do
   result <- TH.runIO $ runExceptT (corba (CorbaInput srv mcn))
   case result of
     Left err ->
       fail . T.unpack $
         renderCorbaError err
     Right res ->
       f res

withService :: FilePath -> [FilePath] -> (CorbaResult -> [Dec]) -> Q [Dec]
withService srv mcn f = do
  loadService' srv mcn (return . f)
