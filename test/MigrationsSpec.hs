module MigrationsSpec where

import Migration (addMigration, getCurrentTimeInFormat, migrateAll, updateMigrations)
import Qtility
import Qtility.Database.Migration (migrationsInDirectory)
import Qtility.Database.Types
  ( Migration (..),
    _MigrationIncorrectFilename,
    _MigrationIncorrectFormat,
  )
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
      let migration1Text =
            mconcat ["SELECT 1 + 1;\n\n", "-- DOWN", "\n\n", "SELECT 1 - 1;\n"]
      files <-
        [("migrations", Map.fromList [("2022-10-28_22-53-45_-_test1.sql", migration1Text)])]
          & Map.fromList
          & newIORef
          & liftIO
      currentTime <- liftIO getCurrentTime >>= newIORef
      migrationsRef <- liftIO $ newIORef mempty
      let migration1 =
            Migration
              { _migrationFilename = "2022-10-28_22-53-45_-_test1.sql",
                _migrationUpStatement = "SELECT 1 + 1;",
                _migrationDownStatement = "SELECT 1 - 1;",
                _migrationTimestamp =
                  Time.UTCTime (Time.fromGregorian 2022 10 28) (22 * 3600 + 53 * 60 + 45),
                _migrationIsApplied = False
              }
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateIsVerbose = isVerbose,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined,
                _testStateMigrations = migrationsRef
              }
      currentTimeString <- runRIO testState getCurrentTimeInFormat

      migrationsInPath1 <- runRIO testState $ migrationsInDirectory "migrations"
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

      migrationsInPath2 <- runRIO testState $ migrationsInDirectory "migrations"
      length migrationsInPath2 `shouldBe` 2

      runRIO testState $ updateMigrations $ MigrationsPath "migrations"
      currentMigrations <- runRIO testState $ readIORef migrationsRef
      length currentMigrations `shouldBe` 2

      liftIO (readIORef outputLines)
        `shouldReturn` Vector.fromList
          [ mconcat
              [ "Created migration '",
                currentTimeString,
                "_-_migration-name.sql'"
              ],
            "Inserted migration: 2022-10-28_22-53-45_-_test1.sql",
            mconcat ["Inserted migration: ", currentTimeString, "_-_migration-name.sql"]
          ]

    it "should output the same lines as in `updateMigrations` when we `migrate`" $ do
      outputLines <- liftIO $ newIORef mempty
      styling <- liftIO $ newIORef []
      isVerbose <- liftIO $ newIORef False
      let migration1Text =
            mconcat ["SELECT 1 + 1;\n\n", "-- DOWN", "\n\n", "SELECT 1 - 1;\n"]
      files <-
        [("migrations", Map.fromList [("2022-10-28_22-53-45_-_test1.sql", migration1Text)])]
          & Map.fromList
          & newIORef
          & liftIO
      currentTime <- liftIO getCurrentTime >>= newIORef
      migrationsRef <- liftIO $ newIORef mempty
      let migration1 =
            Migration
              { _migrationFilename = "2022-10-28_22-53-45_-_test1.sql",
                _migrationUpStatement = "SELECT 1 + 1;",
                _migrationDownStatement = "SELECT 1 - 1;",
                _migrationTimestamp =
                  Time.UTCTime (Time.fromGregorian 2022 10 28) (22 * 3600 + 53 * 60 + 45),
                _migrationIsApplied = False
              }
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateIsVerbose = isVerbose,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined,
                _testStateMigrations = migrationsRef
              }
      currentTimeString <- runRIO testState getCurrentTimeInFormat

      migrationsInPath1 <- runRIO testState $ migrationsInDirectory "migrations"
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

      migrationsInPath2 <- runRIO testState $ migrationsInDirectory "migrations"
      length migrationsInPath2 `shouldBe` 2

      runRIO testState $ migrateAll $ MigrationsPath "migrations"
      currentMigrations <- runRIO testState $ readIORef migrationsRef
      length currentMigrations `shouldBe` 2

      liftIO (readIORef outputLines)
        `shouldReturn` Vector.fromList
          [ mconcat
              [ "Created migration '",
                currentTimeString,
                "_-_migration-name.sql'"
              ],
            "Inserted migration: 2022-10-28_22-53-45_-_test1.sql",
            mconcat ["Inserted migration: ", currentTimeString, "_-_migration-name.sql"]
          ]

    it "Should fail to read a badly named migration file" $ do
      outputLines <- liftIO $ newIORef mempty
      styling <- liftIO $ newIORef []
      isVerbose <- liftIO $ newIORef False
      let migration1Text =
            mconcat ["SELECT 1 + 1;\n\n", "-- DOWN", "\n\n", "SELECT 1 - 1;\n"]
      files <-
        [("migrations", Map.fromList [("20221028_22-53-45_-_test1.sql", migration1Text)])]
          & Map.fromList
          & newIORef
          & liftIO
      currentTime <- liftIO getCurrentTime >>= newIORef
      migrationsRef <- liftIO $ newIORef mempty
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateIsVerbose = isVerbose,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined,
                _testStateMigrations = migrationsRef
              }

      runRIO testState (updateMigrations $ MigrationsPath "migrations")
        `shouldThrow` isA @SomeException _MigrationIncorrectFilename

    it "Should fail to read a badly formatted migration file" $ do
      outputLines <- liftIO $ newIORef mempty
      styling <- liftIO $ newIORef []
      isVerbose <- liftIO $ newIORef False
      let migration1Text =
            -- `-- DOWN` is missing
            mconcat ["SELECT 1 + 1;\n\n", "\n\n", "SELECT 1 - 1;\n"]
      files <-
        [("migrations", Map.fromList [("2022-10-28_22-53-45_-_test1.sql", migration1Text)])]
          & Map.fromList
          & newIORef
          & liftIO
      currentTime <- liftIO getCurrentTime >>= newIORef
      migrationsRef <- liftIO $ newIORef mempty
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateIsVerbose = isVerbose,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined,
                _testStateMigrations = migrationsRef
              }

      runRIO testState (updateMigrations $ MigrationsPath "migrations")
        `shouldThrow` isA @SomeException _MigrationIncorrectFormat

isA :: Prism' a b -> a -> Bool
isA p v = v ^? p & isJust
