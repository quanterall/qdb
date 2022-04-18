module Run (run) where

import Database.PostgreSQL.Simple (ConnectInfo (..))
import Database.PostgreSQL.Simple.Utilities (createConnectionPool)
import Migration
  ( addMigration,
    listMigrations,
    migrateAll,
    removeMigration',
    rollback,
    updateMigrations,
  )
import Qtility
import RIO.Process (mkDefaultProcessContext)
import Types

run :: Options -> AppCommand -> IO ()
run options command = do
  lo <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  let connectionInfo =
        ConnectInfo
          { connectHost = options ^. optionsHost,
            connectPort = options ^. optionsPort,
            connectUser = options ^. optionsUser,
            connectPassword = options ^. optionsPassword,
            connectDatabase = options ^. optionsDatabase
          }
  pool <- createConnectionPool 10 connectionInfo
  withLogFunc lo $ \lf -> do
    let app =
          App
            { _appLogFunc = lf,
              _appProcessContext = pc,
              _appOptions = options,
              _appSqlPool = pool
            }
    runRIO app $ runCommand (options ^. optionsVerbose) command

runCommand :: Bool -> AppCommand -> RIO App ()
runCommand _verbose Migrate = migrateAll
runCommand _verbose (Rollback n) = rollback n
runCommand _verbose (AddMigration name) = addMigration name
runCommand _verbose UpdateMigrations = updateMigrations
runCommand verbose ListMigrations = listMigrations verbose
runCommand _verbose (RemoveMigration filename) = removeMigration' filename
