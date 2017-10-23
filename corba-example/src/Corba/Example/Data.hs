{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Corba.Example.Data where


import           Corba.Core.TH (withService)
import qualified Corba.Codegen.Data as Data
import           Data.Bool (Bool)
import           Data.Text (Text)


$(withService "service/example.corba" ["service/example.mcn"] Data.generateDataV1)
