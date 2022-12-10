{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------

-- | Contains the statically dispatched effect, and relevant functions for a
-- single connection-based DB effect. If you need to take a connection from a
-- pool of connections, you could look into @hasql-pool-effectful@ instead.
--
-- This adapts the @hasql@ package.
module Effectful.Hasql (DB', query, runDB') where

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

{- | Used to determine the dispatch type, and whether or not it's permitted to
 perform side effects.
-}
type instance DispatchOf DB' = 'Static 'WithSideEffects

{- | Contains the initial representation of the @DB'@ effect, which is a single
 DB connection.
-}
newtype instance StaticRep DB' = DB' Connection

-------------------------------------------------------------------------------

-- | Run a @DB'@ effect with the initial representation/environment: a DB
-- connection.
runDB' ::
  forall (es :: [Effect]) (a :: Type).
  IOE :> es =>
  -- | Will be put in the initial environment. Represents a single DB connection
  Connection ->
  Eff (DB' : es) a ->
  Eff es a
runDB' = Static.evalStaticRep . DB'

unDB :: StaticRep DB' -> Connection
unDB (DB' connection) = connection

{- | Executes a DB session

 >>> import qualified Hasql.Session
 >>> import Hasql.TH (singletonStatement)
 >>> import Effectful (runEff)

 >>> session = Session.statement () [singletonStatement | SELECT 1 :: INT |]
 >>> runEff . runDB' connection $ query session
 Right 1
-}
query ::
  forall (es :: [Effect]) (a :: Type).
  (DB' :> es) =>
  -- | Database session containing one or more statements
  Session a ->
  Eff es (Either QueryError a)
query session =
  Static.getStaticRep
    >>= Static.unsafeEff_
      . Session.run session
      . unDB
