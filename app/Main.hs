{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Database.PostgreSQL.Simple (ConnectInfo (..))
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
      addCommand
        "migrate"
        "Apply all unapplied migrations"
        id
        (Migrate <$> parseMigrationsPath <*> parseConnectInfo)
      addCommand
        "rollback"
        "Roll back an amount of migrations"
        id
        (Rollback <$> argument auto (metavar "AMOUNT") <*> parseConnectInfo)
      addCommand
        "add-migration"
        "Add a migration in the migrations directory"
        id
        (AddMigration <$> strArgument (metavar "MIGRATION_NAME") <*> parseMigrationsPath)
      addCommand
        "list-migrations"
        "List all migrations in the database"
        id
        (ListMigrations <$> parseConnectInfo)
      addCommand
        "update-migrations"
        "Update the migrations in the database to match your migrations directory"
        id
        (UpdateMigrations <$> parseMigrationsPath <*> parseConnectInfo)
      addCommand
        "remove-migration"
        "Remove a migration from the database by filename"
        id
        (RemoveMigration <$> strArgument (metavar "MIGRATION_NAME") <*> parseConnectInfo)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output?"
      )

parseMigrationsPath :: Parser MigrationsPath
parseMigrationsPath =
  MigrationsPath
    <$> strOption
      ( long "migrations"
          <> short 'm'
          <> metavar "MIGRATIONS_PATH"
          <> help "Migrations directory"
      )

parseConnectInfo :: Parser ConnectInfo
parseConnectInfo =
  ConnectInfo
    <$> strOption
      ( long "host"
          <> short 'H'
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
      ( long "password"
          <> short 'p'
          <> metavar "PASSWORD"
          <> help "Password to use"
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "DATABASE"
          <> help "Database to use"
      )
