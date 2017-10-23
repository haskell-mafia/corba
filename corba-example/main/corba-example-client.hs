{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.Trans.Except (runExceptT)

import qualified Corba.Example.Client as Client
import           Corba.Example.Data
import           Corba.Runtime.Client

import           Data.Either (Either (..))
import           Data.Function (($), (.), flip)
import           Data.Functor (fmap)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Network.HTTP.Client as HTTP.Client

import qualified System.Environment as Environment
import           System.IO (IO)
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  args <- Environment.getArgs
  mgr <- HTTP.Client.newManager HTTP.Client.defaultManagerSettings
  let
    initRequest req =
      req { HTTP.Client.port = 8080, HTTP.Client.path = "/rpc" }
    client =
      Client.exampleClientJsonV1 $
        RequestModifier (flip HTTP.Client.httpLbs mgr . initRequest)

  r <- runExceptT $ ping client (PingRequestV1 . T.intercalate " " . fmap T.pack $ args)

  case r of
    Left e ->
      IO.print e
    Right (PingResponseV1 msg) ->
      T.putStrLn msg
