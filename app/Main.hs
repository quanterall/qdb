{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Options.Applicative.Simple
import qualified Paths_qdb
import Qtility
import RIO.Process
import Run
import Types

main :: IO ()
main = do
  (options, command) <-
    simpleOptions
      $(simpleVersion Paths_qdb.version)
      "qdb"
      "Tool for managing databases: migrations, etc."
      parseOptions
      addCommands
  run options command
  where
    addCommands = do
      addCommand "migrate" "Migrate the database" id (pure Migrate)

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
