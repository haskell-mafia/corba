{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Corba.Example.Service where


import           Control.Monad.Trans.Except (ExceptT)

import           Corba.Core.TH (withService)
import qualified Corba.Codegen.Wai.Json as Wai
import           Corba.Example.Data
import           Corba.Example.Json
import           Corba.Runtime.Core.Data
import           Corba.Runtime.Wai.Data
import           Corba.Runtime.Wai.Json


$(withService "service/example.corba" ["service/example.mcn"] Wai.generateWaiV1)
