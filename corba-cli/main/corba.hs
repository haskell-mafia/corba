{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           BuildInfo_corba_cli
import           DependencyInfo_corba_cli

import           Control.Monad.IO.Class (liftIO)

import           Corba.Core
import qualified Corba.Codegen.Aeson as Aeson
import qualified Corba.Codegen.Data as Data
import qualified Corba.Codegen.Wai.Json as Wai

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative (Parser)
import qualified Options.Applicative as O

import           P

import           System.Directory
import qualified System.FilePath.Glob as Glob
import           System.IO (IO, FilePath)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (dispatch, safeCommand, RunType (..), SafeCommand (..))


{--
Usage: corba service.corba -d 'data/**/*.mcn'
--}

data CorbaCliError =
    CorbaCliError
  | GlobError Text
  | CorbaError CorbaError


renderCorbaCliError :: CorbaCliError -> Text
renderCorbaCliError cce =
  case cce of
    CorbaCliError ->
      "Corba CLI error"
    GlobError e ->
      "Invalid glob: " <> e
    CorbaError e ->
      renderCorbaError e

data CorbaOpts = CorbaOpts {
    _optServiceFile :: FilePath
  , _optDataGlob :: Glob
  } deriving (Eq, Ord, Show)

newtype Glob = Glob FilePath
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  cmd <- dispatch (safeCommand corbaP)
  case cmd of
    VersionCommand ->
      T.putStrLn ("corba2000-" <> (T.pack buildInfoVersion))
    DependencyCommand ->
      traverse_ (T.putStrLn . T.pack) dependencyInfo
    RunCommand DryRun c ->
      IO.print c
    RunCommand RealRun c ->
      run c

run :: CorbaOpts -> IO ()
run (CorbaOpts service dataGlob) =
  orDie renderCorbaCliError $ do
    dfs <- globSafe dataGlob
    let
      cinput = CorbaInput {
          corbaService = service
        , corbaData = dfs
        }
    out <- firstT CorbaError $ corba cinput
    liftIO . T.putStrLn $ Aeson.generateAesonModuleV1 out
    liftIO . T.putStrLn $ Data.generateDataModuleV1 out
    -- FIXME this produces an invalid module
    liftIO . T.putStrLn $ Wai.generateWaiModuleV1 out

-- -----------------------------------------------------------------------------

-- There's some partial code in Glob, you wanna be careful with that.
globSafe :: Glob -> EitherT CorbaCliError IO [FilePath]
globSafe (Glob g) = do
  pat <- hoistEither (first (GlobError . T.pack)
           (Glob.tryCompileWith Glob.compDefault {Glob.errorRecovery = False} g))
  -- This is not Windows-safe, I believe you'd need to remove the Drive prefix.
  cwd <- liftIO getCurrentDirectory
  liftIO (Glob.globDir1 pat cwd)

-- -----------------------------------------------------------------------------

corbaP :: Parser CorbaOpts
corbaP =
  CorbaOpts
    <$> O.argument O.str (O.metavar "SERVICE")
    <*> dataP

dataP :: Parser Glob
dataP =
  fmap Glob . O.option O.str $ fold [
      O.short 'd'
    , O.long "data"
    , O.metavar "DATA_FILES"
    ]
