{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Yaml (decodeThrow)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.AWS.QAWS.SecretsManager.Types (SecretARN (..))
import Options.Applicative.Simple
import qualified Paths_qdb
import Qtility
import qualified RIO.ByteString as ByteString
import qualified RIO.Text as Text
import Run
import qualified System.Environment as Environment
import Types

main :: IO ()
main = do
  configurationPathFromEnvironment <-
    (Just <$> readEnvironmentVariable "QDB_CONFIG")
      `catch` \(_e :: ReadEnvironmentVariableError) -> pure $ Just ".qdb.yaml"
  arguments <- Environment.getArgs
  let configurationPath =
        fromMaybe ".qdb.yaml" $ findConfigurationPath arguments <|> configurationPathFromEnvironment
  configurationOptions <-
    readConfigurationFile configurationPath `catchIO` handleConfigurationFileError
  (options, command') <-
    simpleOptions
      $(simpleVersion Paths_qdb.version)
      "qdb"
      "Tool for managing databases: migrations, etc."
      parseOptions
      (addCommands configurationOptions)
  run options command'
  where
    handleConfigurationFileError _exception =
      pure
        ConfigurationFileOptions
          { _configurationFileOptionsMigrationsPath = Nothing,
            _configurationFileOptionsSecretArn = Nothing,
            _configurationFileOptionsHost = Nothing,
            _configurationFileOptionsPort = Nothing,
            _configurationFileOptionsUser = Nothing,
            _configurationFileOptionsPassword = Nothing,
            _configurationFileOptionsDatabase = Nothing
          }

    addCommands options = do
      addCommand
        "migrate"
        "Apply all unapplied migrations"
        id
        ( Migrate
            <$> parseMigrationsPath options
            <*> parseConnectionInfo options
        )

      addCommand
        "rollback"
        "Roll back an amount of migrations"
        id
        (Rollback <$> argument auto (metavar "AMOUNT" <> value 1) <*> parseConnectionInfo options)

      addCommand
        "add-migration"
        "Add a migration in the migrations directory"
        id
        ( AddMigration <$> strArgument (metavar "MIGRATION_NAME")
            <*> parseMigrationsPath options
        )

      addCommand
        "list-migrations"
        "List all migrations in the database"
        id
        (ListMigrations <$> parseConnectionInfo options)

      addCommand
        "update-migrations"
        "Update the migrations in the database to match your migrations directory"
        id
        ( UpdateMigrations
            <$> parseMigrationsPath options
            <*> parseConnectionInfo options
        )

      addCommand
        "remove-migration"
        "Remove a migration from the database by filename"
        id
        ( RemoveMigration <$> strArgument (metavar "MIGRATION_NAME")
            <*> parseConnectionInfo options
        )

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output?"
      )
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "PATH"
          <> help "Path to the configuration file"
          <> value ".qdb.yaml"
      )

parseMigrationsPath :: ConfigurationFileOptions -> Parser MigrationsPath
parseMigrationsPath options =
  MigrationsPath
    <$> strOption
      ( long "migrations"
          <> short 'm'
          <> metavar "MIGRATIONS_PATH"
          <> help "Migrations directory"
          <> maybe mempty value (options ^. configurationFileOptionsMigrationsPath)
      )

parseConnectionInfo :: ConfigurationFileOptions -> Parser ConnectionInfo
parseConnectionInfo options =
  (RDSConnection <$> parseArn (options ^. configurationFileOptionsSecretArn))
    <|> (ManualConnection <$> parseConnectInfo options)

parseConnectInfo :: ConfigurationFileOptions -> Parser ConnectInfo
parseConnectInfo options =
  ConnectInfo
    <$> strOption
      ( long "host"
          <> short 'H'
          <> metavar "HOST"
          <> help "Host to connect to"
          <> maybe mempty value (options ^. configurationFileOptionsHost)
      )
    <*> option
      auto
      ( long "port"
          <> short 'P'
          <> metavar "PORT"
          <> help "Port to connect to"
          <> value (fromMaybe 5432 (options ^. configurationFileOptionsPort))
      )
    <*> strOption
      ( long "user"
          <> short 'u'
          <> metavar "USER"
          <> help "User to connect as"
          <> maybe mempty value (options ^. configurationFileOptionsUser)
      )
    <*> strOption
      ( long "password"
          <> short 'p'
          <> metavar "PASSWORD"
          <> help "Password to use"
          <> maybe mempty value (options ^. configurationFileOptionsPassword)
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "DATABASE"
          <> help "Database to use"
          <> maybe mempty value (options ^. configurationFileOptionsDatabase)
      )

parseArn :: Maybe String -> Parser SecretARN
parseArn maybeArn =
  SecretARN
    <$> strOption
      ( long "secret-arn"
          <> short 's'
          <> metavar "SECRET_ARN"
          <> help "Secret ARN to use for getting connection info"
          <> maybe mempty (Text.pack >>> value) maybeArn
      )

readConfigurationFile :: FilePath -> IO ConfigurationFileOptions
readConfigurationFile configurationPath = do
  bytes <- ByteString.readFile configurationPath
  decodeThrow bytes

findConfigurationPath :: [String] -> Maybe FilePath
findConfigurationPath [] = Nothing
findConfigurationPath (argument' : arguments)
  | argument' == "--config" =
    case arguments of
      [] -> Nothing
      (configurationPath : _) -> Just configurationPath
  | argument' == "-c" =
    case arguments of
      [] -> Nothing
      (configurationPath : _) -> Just configurationPath
  | otherwise = findConfigurationPath arguments
