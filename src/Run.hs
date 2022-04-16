module Run (run) where

import Database.PostgreSQL.Simple (ConnectInfo (..))
import Database.PostgreSQL.Simple.Utilities (createConnectionPool)
import Migration (addMigration, migrateAll, rollback)
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
    runRIO app $ runCommand command

runCommand :: AppCommand -> RIO App ()
runCommand Migrate = migrateAll
runCommand (Rollback n) = rollback n
runCommand (AddMigration name) = addMigration name
