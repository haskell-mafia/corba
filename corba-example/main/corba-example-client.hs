{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative (pure)
import           Control.Monad.Trans.Except (runExceptT)

import           Corba.Example.Client
import           Corba.Runtime.Client

import           Data.Either (Either (..))
import           Data.Function (($), (.), flip)

import           Network.HTTP.Client as HTTP.Client

import           System.IO (IO)
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  mgr <- HTTP.Client.newManager HTTP.Client.defaultManagerSettings
  let
    initRequest req =
      req { HTTP.Client.port = 8080, HTTP.Client.path = "/rpc" }
  r <- runExceptT . method1 $
    RequestModifier (flip HTTP.Client.httpLbs mgr . initRequest)
  case r of
    Left e ->
      IO.print e
    Right () ->
      pure ()
