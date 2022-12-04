{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Hasql.Effect (runDB, query, DB) where

import Data.Kind (Type)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep)
import qualified Effectful.Dispatch.Static as Static
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, Session)
import qualified Hasql.Session as Session

data DB :: Effect

type instance DispatchOf DB = 'Static 'WithSideEffects
newtype instance StaticRep DB = DB Connection

query ::
  forall (es :: [Effect]) (a :: Type).
  (DB :> es) =>
  Session a ->
  Eff es (Either QueryError a)
query session =
  Static.getStaticRep >>= Static.unsafeEff_ . Session.run session . unDB
  where unDB (DB connection) = connection

runDB ::
  forall (es :: [Effect]) (a :: Type).
  IOE :> es =>
  Connection ->
  Eff (DB : es) a ->
  Eff es a
runDB = Static.evalStaticRep . DB
