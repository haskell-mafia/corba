{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba.Syntax.Service (
    ServiceParseError
  , renderServiceParseError
  , parseService
  , printService
  ) where


import           Corba.Data.Service

import qualified Data.Text as T

import           P

import           Text.Read (readMaybe)
import           Text.Show.Pretty (ppShow)


-- FIXME
-- THIS IS NOT REAL SYNTAX
-- THIS IS A PLACEHOLDER IN A PROTOTYPE
-- IF THIS CODE IS BEING USED IN 2018, YOU HAVE FAILED

data ServiceParseError =
    ServiceParseError
  deriving (Eq, Ord, Show)

renderServiceParseError :: ServiceParseError -> Text
renderServiceParseError se =
  case se of
    ServiceParseError ->
      "Failed to read service definition - write a real parser if you want real errors"

parseService :: Text -> Either ServiceParseError Service
parseService =
  maybe (Left ServiceParseError) Right . readMaybe . T.unpack

printService :: Service -> Text
printService =
  T.pack . ppShow
