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
  app <- createApp options connectInfo
  runRIO app $ migrateAll migrationsPath
run options (Rollback migrationsPath connectInfo) = do
  app <- createApp options connectInfo
  runRIO app $ rollback migrationsPath
run options (UpdateMigrations migrationsPath connectInfo) = do
  app <- createApp options connectInfo
  runRIO app $ updateMigrations migrationsPath
run options (ListMigrations connectInfo) = do
  app <- createApp options connectInfo
  runRIO app $ listMigrations $ options ^. optionsVerbose
run options (RemoveMigration name connectInfo) = do
  app <- createApp options connectInfo
  runRIO app $ removeMigration' name

createApp :: Options -> ConnectionInfo -> IO App
createApp options connectionInfo = do
  lo <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  pool <- case connectionInfo of
    ManualConnection connectInfo -> createConnectionPool 10 connectInfo
    RDSConnection secretArn -> do
      awsEnv <- loadAWSEnvironment ".env"
      createConnectionPoolForSecretArn' awsEnv 10 secretArn
  withLogFunc lo $ \lf -> do
    pure App {_appLogFunc = lf, _appProcessContext = pc, _appOptions = options, _appSqlPool = pool}
