{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Int (Int32)
import Effectful (Eff, runEff, type (:>))
import Effectful.Hasql.Pool (DB, runDB, use)
import qualified Hasql.Connection as Connection
import Hasql.Pool (UsageError, acquire)
import qualified Hasql.Session as Transaction
import Hasql.TH (singletonStatement)

-- TODO: Write actual tests lol
main :: IO ()
main = do
  let settings = Connection.settings "localhost" 5432 "postgres" "" "postgres"
  pool <- acquire 1 Nothing settings

  a <- runEff . runDB pool $ actions

  print a
  pure ()

actions :: DB :> es => Eff es (Either UsageError Int32)
actions = do
  let session = Transaction.statement () [singletonStatement| select 1 :: int |]

  use session
