{- WARNING: This file is/will-be generated -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Example.Service where


import           Control.Applicative (pure)

import           Corba.Runtime.Core.Data
import           Corba.Runtime.Wai.Data
import           Corba.Runtime.Wai.Json

methods :: [RpcHandler]
methods =
  [jsonV1 [method1]]

method1 :: JsonMethod
method1 =
  JsonMethod
    (MethodName "a")
    (\_ -> pure ())
    (\() -> pure ())
    (\() -> "")
