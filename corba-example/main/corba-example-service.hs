{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Corba.Example.Service
import           Corba.Runtime.Wai

import           Data.Function (($), (.))

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai.RequestLogger

import           System.IO (IO)
import qualified System.IO as IO


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  Warp.runSettings (Warp.setPort 8080 Warp.defaultSettings) $
    Wai.RequestLogger.logStdout . middleware ["rpc"] methods $
    notFoundApplication

notFoundApplication :: Wai.Application
notFoundApplication _req resp =
  resp $
    Wai.responseBuilder
      HTTP.status404
      [(HTTP.hContentType, "text/html; charset=utf-8")]
      "<html><body>Not found</body></html>"
