{- WARNING: This file is/will-be generated -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Example.Client where


import           Control.Applicative (pure)
import           Control.Monad.Trans.Except (ExceptT)

import           Corba.Runtime.Client
import           Corba.Runtime.Client.Json
import           Corba.Runtime.Core.Data

import           System.IO (IO)


method1 :: RequestModifier IO -> ExceptT ClientError IO ()
method1 m =
  jsonRequest
    m
    (MethodName "a")
    (\() -> "")
    (\_ -> pure ())
    ()
