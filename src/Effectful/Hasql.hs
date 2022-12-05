{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------

module Effectful.Hasql (DB', query, runDB) where

-------------------------------------------------------------------------------

import Data.Kind (Type)

-------------------------------------------------------------------------------

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep)
import qualified Effectful.Dispatch.Static as Static
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, Session)
import qualified Hasql.Session as Session

-------------------------------------------------------------------------------

-- | A single connection DB effect
data DB' :: Effect

type instance DispatchOf DB' = 'Static 'WithSideEffects
newtype instance StaticRep DB' = DB' Connection

-------------------------------------------------------------------------------

runDB ::
  forall (es :: [Effect]) (a :: Type).
  IOE :> es =>
  Connection ->
  Eff (DB' : es) a ->
  Eff es a
runDB = Static.evalStaticRep . DB'

unDB :: StaticRep DB' -> Connection
unDB (DB' connection) = connection

query ::
  forall (es :: [Effect]) (a :: Type).
  (DB' :> es) =>
  Session a ->
  Eff es (Either QueryError a)
query session =
  Static.getStaticRep
    >>= Static.unsafeEff_
      . Session.run session
      . unDB