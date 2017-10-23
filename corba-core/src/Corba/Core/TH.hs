{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Core.TH where


import           Control.Monad.Trans.Except (runExceptT)

import           Corba.Core

import qualified Data.Text as T

import           Language.Haskell.TH (Q, Exp)
import qualified Language.Haskell.TH as TH

import           P

import           System.IO (FilePath)

import           X.Language.Haskell.TH (dataExp)


loadService :: FilePath -> [FilePath] -> Q Exp
loadService srv mcn = do
   result <- TH.runIO $ runExceptT (corba (CorbaInput srv mcn))
   case result of
     Left err ->
       fail . T.unpack $
         renderCorbaError err
     Right res ->
       dataExp res
