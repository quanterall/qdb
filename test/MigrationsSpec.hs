module MigrationsSpec where

import Migration (addMigration, getCurrentTimeInFormat, listMigrations, updateMigrations)
import Migration.Class (ReadMigrations (..))
import Qtility
import Qtility.Database.Types (Migration (..))
import qualified RIO.Map as Map
import RIO.Time (getCurrentTime)
import qualified RIO.Time as Time
import qualified RIO.Vector as Vector
import Test.Hspec
import TestUtilities (TestState (..))
import Types (MigrationsPath (..))

spec :: Spec
spec = do
  describe "Migrations" $ do
    it "should output lines to the terminal when we execute migration functions" $ do
      outputLines <- liftIO $ newIORef mempty
      styling <- liftIO $ newIORef []
      isVerbose <- liftIO $ newIORef False
      files <- liftIO $ newIORef mempty
      currentTime <- liftIO getCurrentTime >>= newIORef
      migrationsRef <- liftIO $ newIORef mempty
      let migration1 =
            Migration
              { _migrationFilename = "test1.sql",
                _migrationUpStatement = "",
                _migrationDownStatement = "",
                _migrationTimestamp = Time.UTCTime (Time.fromGregorian 2022 10 28) 0,
                _migrationIsApplied = False
              }
      migrationsInDirectoryRef <-
        [(MigrationsPath "migrations", [migration1])] & Map.fromList & newIORef
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateIsVerbose = isVerbose,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined,
                _testStateMigrations = migrationsRef,
                _testStateMigrationsInDirectory = migrationsInDirectoryRef
              }
      currentTimeString <- runRIO testState getCurrentTimeInFormat

      migrationsInPath1 <- runRIO testState $ migrationsInDirectoryM (MigrationsPath "migrations")
      migrationsInPath1 `shouldBe` [migration1]

      runRIO testState $ do
        addMigration "migration-name" $ MigrationsPath "migrations"

      liftIO (readIORef outputLines)
        `shouldReturn` Vector.fromList
          [ mconcat
              [ "Created migration '",
                currentTimeString,
                "_-_migration-name.sql'"
              ]
          ]

      migrationsInPath2 <- runRIO testState $ migrationsInDirectoryM (MigrationsPath "migrations")
      traceShowM migrationsInPath2
      length migrationsInPath2 `shouldBe` 2
