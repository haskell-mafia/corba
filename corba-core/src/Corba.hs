{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Corba (
    CorbaError
  , renderCorbaError
  , CorbaInput (..)
  , CorbaResult (..)
  , corba
  ) where


import           Corba.Data.Service
import           Corba.Syntax.Service (ServiceParseError, renderServiceParseError)
import qualified Corba.Syntax.Service as SS

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (UnicodeException)

import           Machinator.Core (MachinatorError, renderMachinatorError)
import qualified Machinator.Core as Machinator

import           P

import           System.IO (FilePath, IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT)


data CorbaError =
    CorbaServiceParseError ServiceParseError
  | CorbaMachinatorParseError MachinatorError
  | CorbaUtf8Error FilePath UnicodeException

renderCorbaError :: CorbaError -> Text
renderCorbaError ce =
  case ce of
    CorbaServiceParseError spe ->
      renderServiceParseError spe
    CorbaMachinatorParseError me ->
      renderMachinatorError me
    CorbaUtf8Error fp ue ->
      T.unlines [
          T.pack fp <> ": File was not UTF-8"
        , T.pack (show ue)
        ]

data CorbaInput = CorbaInput {
    corbaService :: FilePath
  , corbaData :: [FilePath]
  } deriving (Eq, Ord, Show)

data CorbaResult = CorbaResult {
    resultService :: Service
  , resultData :: [Machinator.Definition]
  } deriving (Eq, Ord, Show)

corba :: CorbaInput -> EitherT CorbaError IO CorbaResult
corba (CorbaInput sfile dfiles) = do
  CorbaResult
    <$> parseService sfile
    <*> parseData dfiles

parseService :: FilePath -> EitherT CorbaError IO Service
parseService sfile = do
  t <- readFile sfile
  hoistEither (first CorbaServiceParseError (SS.parseService t))

parseData :: [FilePath] -> EitherT CorbaError IO [Machinator.Definition]
parseData dfiles =
  fmap fold . for dfiles $ \dfile -> do
    t <- readFile dfile
    Machinator.Versioned _v (Machinator.DefinitionFile _fp defs) <-
      firstT CorbaMachinatorParseError $ hoistEither (Machinator.parseDefinitionFile dfile t)
    pure defs


-- XXX

readFile :: FilePath -> EitherT CorbaError IO Text
readFile fp =
  firstT (CorbaUtf8Error fp) $ newEitherT (readFileUtf8 fp)

readFileUtf8 :: FilePath -> IO (Either UnicodeException Text)
readFileUtf8 fp =
  TE.decodeUtf8' <$> B.readFile fp
