{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Corba.Example.Json where


import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Functor
import           Data.Text (Text)

import           Corba.Core.TH (withService)
import qualified Corba.Codegen.Aeson as Aeson
import           Corba.Example.Data


$(withService "service/example.corba" ["service/example.mcn"] Aeson.generateAesonV1)
