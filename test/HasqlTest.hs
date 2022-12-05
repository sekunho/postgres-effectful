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

import Data.Either (fromRight)
import Data.Int (Int32)

-------------------------------------------------------------------------------

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
    let session = Session.statement () [singletonStatement| SELECT col_404 :: INT from table_404 |]
     in query session

-------------------------------------------------------------------------------
-- Helpers

setup :: IO Connection
setup = acquire settings >>= pure . fromRight (error "kaboom")
 where
  settings = Connection.settings "localhost" 5432 "postgres" "" "postgres"
