{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------

module HasqlTest where

-------------------------------------------------------------------------------

import Data.Int (Int32)
import qualified System.Environment as Environment (lookupEnv)

-------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Effectful (runEff)
import Effectful.Hasql (query, runDB)
import Hasql.Connection (Connection, acquire)
import qualified Hasql.Connection as Connection
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
  describe "select" $ do
    it "is 1" $
      (setup >>= select1) `shouldReturn` Right 1

    it "is dead" $
      (setup >>= badSelect)
        `shouldReturn` Left
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

-------------------------------------------------------------------------------
-- Queries

select1 :: Connection -> IO (Either QueryError Int32)
select1 connection =
  runEff . runDB connection $
    let session = Session.statement () [singletonStatement| select 1 :: int |]
     in query session

badSelect :: Connection -> IO (Either QueryError Int32)
badSelect connection =
  runEff . runDB connection $
    let session =
          Session.statement
            ()
            [singletonStatement| SELECT col_404 :: INT from table_404 |]
     in query session

-------------------------------------------------------------------------------
-- Helpers

setup :: IO Connection
setup = do
  testEnv <- getTestEnv
  connection <- acquire (mkSettings testEnv)

  case connection of
    Right connection' -> pure connection'
    Left e -> error (show e)
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
