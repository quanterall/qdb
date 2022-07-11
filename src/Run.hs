module Run (run) where

import Migration
  ( addMigration,
    listMigrations,
    migrateAll,
    removeMigration',
    rollback,
    updateMigrations,
  )
import Network.AWS.QAWS
import Network.AWS.QAWS.SecretsManager (createConnectionPoolForSecretArn')
import Qtility
import Qtility.Database (createConnectionPool)
import RIO.Process (mkDefaultProcessContext)
import Types

run :: Options -> AppCommand -> IO ()
run _options (AddMigration name migrationsPath) = addMigration name migrationsPath
run options (Migrate migrationsPath connectInfo) = do
  (app, teardown) <- createApp options connectInfo
  runRIO app $ migrateAll migrationsPath
  teardown
run options (Rollback migrationsPath connectInfo) = do
  (app, teardown) <- createApp options connectInfo
  runRIO app $ rollback migrationsPath
  teardown
run options (UpdateMigrations migrationsPath connectInfo) = do
  (app, teardown) <- createApp options connectInfo
  runRIO app $ updateMigrations migrationsPath
  teardown
run options (ListMigrations connectInfo) = do
  (app, teardown) <- createApp options connectInfo
  runRIO app $ listMigrations $ options ^. optionsVerbose
  teardown
run options (RemoveMigration name connectInfo) = do
  (app, teardown) <- createApp options connectInfo
  runRIO app $ removeMigration' name
  teardown

createApp :: Options -> ConnectionInfo -> IO (App, IO ())
createApp options connectionInfo = do
  logOptions <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  pool <- case connectionInfo of
    ManualConnection connectInfo -> createConnectionPool 10 connectInfo
    RDSConnection secretArn -> do
      awsEnv <- loadAWSEnvironment ".env"
      createConnectionPoolForSecretArn' awsEnv 10 secretArn
  (logFunc, teardown) <- newLogFunc @IO @IO logOptions
  pure
    ( App
        { _appLogFunc = logFunc,
          _appProcessContext = pc,
          _appOptions = options,
          _appSqlPool = pool
        },
      teardown
    )
