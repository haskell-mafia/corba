{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)

import           Corba.Example.Data
import qualified Corba.Example.Service as Service
import           Corba.Runtime.Core.Data
import           Corba.Runtime.Wai

import           Data.Function (($), (.))
import           Data.Int (Int)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai.RequestLogger

import qualified System.Environment as Environment
import           System.IO (IO)
import qualified System.IO as IO

import           Text.Read (readMaybe)


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  port <- portEnv
  Warp.runSettings (Warp.setPort port Warp.defaultSettings) $
    Wai.RequestLogger.logStdout . middleware ["rpc"] [Service.exampleJsonV1 businessLogic] $
    notFoundApplication

businessLogic :: ExampleService (ExceptT ErrorMessage IO)
businessLogic =
  ExampleService {
      ping = ping'
    }

ping' :: PingRequest -> ExceptT ErrorMessage IO PingResponse
ping' (PingRequestV1 msg) = do
  liftIO $ T.putStrLn msg
  return $ PingResponseV1 (T.reverse msg)

notFoundApplication :: Wai.Application
notFoundApplication _req resp =
  resp $
    Wai.responseBuilder
      HTTP.status404
      [(HTTP.hContentType, "text/html; charset=utf-8")]
      "<html><body>Not found</body></html>"

portEnv :: IO Int
portEnv = do
  mp <- Environment.lookupEnv "PORT"
  return $ fromMaybe 8080 (mp >>= readMaybe)
