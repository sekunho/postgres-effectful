{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------

module HasqlPoolTest where

-------------------------------------------------------------------------------

import Data.Int (Int32)
import qualified System.Environment as Environment (lookupEnv)

-------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Effectful (runEff)
import Effectful.Hasql.Pool
import Hasql.Connection (Connection, Settings, acquire)
import qualified Hasql.Connection as Connection
import Hasql.Pool (Pool, UsageError (SessionUsageError))
import qualified Hasql.Pool as Pool
import Hasql.Session (
  CommandError (ResultError),
  QueryError (QueryError),
  ResultError (ServerError),
 )
import qualified Hasql.Session as Session
import Hasql.TH (singletonStatement)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Tasty
import Test.Tasty.Hspec (testSpec)

-------------------------------------------------------------------------------

data TestEnv = MkTestEnv
  { _dbHost :: ByteString
  , _dbUser :: ByteString
  , _dbPassword :: ByteString
  , _dbName :: ByteString
  }
  deriving (Show)

-------------------------------------------------------------------------------

test_select :: IO TestTree
test_select = testSpec "test" selectSpec

selectSpec :: Spec
selectSpec = do
  describe "select using pool" $ do
    it "is 1" $
      (setup >>= select1) `shouldReturn` Right 1

    it "is dead" $
      (setup >>= badSelect)
        `shouldReturn` Left
          ( SessionUsageError
              ( QueryError
                  "SELECT col_404 :: INT FROM table_404"
                  []
                  ( ResultError
                      ( ServerError
                          "42P01"
                          "relation \"table_404\" does not exist"
                          Nothing
                          Nothing
                          (Just 28)
                      )
                  )
              )
          )

-------------------------------------------------------------------------------
-- Queries

select1 :: Pool -> IO (Either UsageError Int32)
select1 pool =
  runEff . runDB pool $
    let session = Session.statement () [singletonStatement| select 1 :: int |]
     in use session

badSelect :: Pool -> IO (Either UsageError Int32)
badSelect pool =
  runEff . runDB pool $
    let session =
          Session.statement
            ()
            [singletonStatement| SELECT col_404 :: INT from table_404 |]
     in use session

-------------------------------------------------------------------------------
-- Helpers

getPool :: Settings -> Int -> IO Pool
getPool settings poolCapacity = Pool.acquire poolCapacity Nothing settings

setup :: IO Pool
setup = do
  testEnv <- getTestEnv
  pool <- getPool (mkSettings testEnv) 1

  pure pool
 where
  mkSettings (MkTestEnv dbHost dbUser dbPassword dbName) =
    Connection.settings dbHost 5432 dbUser dbPassword dbName

  getTestEnv :: IO TestEnv
  getTestEnv = do
    pgHost <- Environment.lookupEnv "POSTGRES_HOST"
    pgUser <- Environment.lookupEnv "POSTGRES_USER"
    pgPassword <- Environment.lookupEnv "POSTGRES_PASSWORD"
    pgDbName <- Environment.lookupEnv "POSTGRES_DB_NAME"

    case env pgHost pgUser pgPassword pgDbName of
      Just env' -> pure env'
      Nothing -> error "Lacking test environment variable(s) for Postgres server"
   where
    env pgHost pgUser pgPassword pgDbName =
      MkTestEnv
        <$> (Char8.pack <$> pgHost)
        <*> (Char8.pack <$> pgUser)
        <*> (Char8.pack <$> pgPassword)
        <*> (Char8.pack <$> pgDbName)
