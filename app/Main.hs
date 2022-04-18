{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Options.Applicative.Simple
import qualified Paths_qdb
import Qtility
import Run
import Types

main :: IO ()
main = do
  (options, command') <-
    simpleOptions
      $(simpleVersion Paths_qdb.version)
      "qdb"
      "Tool for managing databases: migrations, etc."
      parseOptions
      addCommands
  run options command'
  where
    addCommands = do
      addCommand "migrate" "Apply all unapplied migrations" id (pure Migrate)
      addCommand
        "rollback"
        "Roll back an amount of migrations"
        id
        (Rollback <$> argument auto (metavar "AMOUNT"))
      addCommand
        "add-migration"
        "Add a migration in the migrations directory"
        id
        (AddMigration <$> strArgument (metavar "MIGRATION_NAME"))

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output?"
      )
    <*> strOption
      ( long "host"
          <> short 'h'
          <> metavar "HOST"
          <> help "Host to connect to"
          <> value "localhost"
      )
    <*> option
      auto
      ( long "port"
          <> short 'P'
          <> metavar "PORT"
          <> help "Port to connect to"
          <> value 5432
      )
    <*> strOption
      ( long "user"
          <> short 'u'
          <> metavar "USER"
          <> help "User to connect as"
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "DATABASE"
          <> help "Database to use"
      )
    <*> strOption
      ( long "password"
          <> short 'p'
          <> metavar "PASSWORD"
          <> help "Password to use"
      )
    <*> strOption
      ( long "migrations"
          <> short 'm'
          <> metavar "MIGRATIONS_PATH"
          <> help "Migrations directory"
      )
