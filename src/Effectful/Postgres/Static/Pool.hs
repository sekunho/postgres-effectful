{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------

-- | Contains the statically dispatched effect, and relevant functions for a
-- pool of connections DB effect. If you only need to handle a single connection,
-- you could look into @hasql-effectful@ instead.
--
-- This adapts the @hasql-pool@ package.
module Effectful.Postgres.Static.Pool (DB, use, runPgPool) where

-------------------------------------------------------------------------------

import Data.Kind (Type)

-------------------------------------------------------------------------------

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep)
import qualified Effectful.Dispatch.Static as Static
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.Session (Session)

-------------------------------------------------------------------------------

-- | Connection pool DB effect
data DB :: Effect

-- | Used to determine the dispatch type, and whether or not it's permitted to
-- perform side effects.
type instance DispatchOf DB = 'Static 'WithSideEffects

-- | Contains the initial representation of the @DB@ effect, which is a DB pool
-- of connections.
newtype instance StaticRep DB = DB Pool

-------------------------------------------------------------------------------

-- | Run a @DB@ effect with the initial representation/environment: a DB pool.
runPgPool ::
  forall (es :: [Effect]) (a :: Type).
  IOE :> es =>
  -- | Will be put in the initial environment. Represents a DB pool of connections
  Pool ->
  Eff (DB : es) a ->
  Eff es a
runPgPool = Static.evalStaticRep . DB

-- | Use a connection from the pool to run a DB session, and returns it to the
-- pool when finished.
use ::
  forall (es :: [Effect]) (a :: Type).
  DB :> es =>
  -- | Session to be executed within the @DB@ environment
  Session a ->
  Eff es (Either UsageError a)
use session =
  Static.getStaticRep
    >>= \(DB pool) -> Static.unsafeEff_ $ Pool.use pool session
