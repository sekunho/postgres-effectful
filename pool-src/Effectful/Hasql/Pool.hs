{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Hasql.Pool (DB, release, use, runDB) where

import Data.Kind (Type)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep)
import qualified Effectful.Dispatch.Static as Static
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.Session (Session)

-- | Connection pool DB effect
data DB :: Effect

type instance DispatchOf DB = 'Static 'WithSideEffects
newtype instance StaticRep DB = DB Pool

runDB ::
  forall (es :: [Effect]) (a :: Type).
  IOE :> es =>
  Pool ->
  Eff (DB : es) a ->
  Eff es a
runDB = Static.evalStaticRep . DB

release :: Pool -> Eff es ()
release = Static.unsafeEff_ . Pool.release

use ::
  forall (es :: [Effect]) (a :: Type).
  DB :> es =>
  Session a ->
  Eff es (Either UsageError a)
use session =
  Static.getStaticRep
    >>= \(DB pool) -> Static.unsafeEff_ $ Pool.use pool session
