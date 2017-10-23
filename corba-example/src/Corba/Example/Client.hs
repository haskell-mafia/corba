{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Corba.Example.Client where


import           Control.Monad.Trans.Except (ExceptT)

import           Corba.Core.TH (withService)
import qualified Corba.Codegen.Client.Json as Client
import           Corba.Example.Data
import           Corba.Example.Json
import           Corba.Runtime.Client
import           Corba.Runtime.Client.Json
import           Corba.Runtime.Core.Data


$(withService "service/example.corba" ["service/example.mcn"] Client.generateClientV1)
