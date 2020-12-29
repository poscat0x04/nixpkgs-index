{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.PooledConc
  ( PooledConc,
    pooledConcToIO,
    pooledConcToUnliftIO,
    Async,
    async,
    withAsync,
    wait,
    A.waitSTM,
    withTaskGroup,
  )
where

import Control.Concurrent.Async.Pool (Async, TaskGroup)
import qualified Control.Concurrent.Async.Pool as A
import Control.Effect
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Internal.Newtype
import Control.Effect.Internal.Utils
import Control.Effect.Unlift
import Control.Monad.Trans.Identity

newtype PooledConc m a = PooledConc (Unlift IO m a)
  deriving (EffNewtype) via PooledConc `WrapperOf` Unlift IO

unliftConc :: Eff PooledConc m => ((forall x. m x -> IO x) -> IO a) -> m a
unliftConc main = wrapWith PooledConc $ unlift (\lower -> main (lower .# lift))
{-# INLINE unliftConc #-}

type PooledConcToIOC =
  CompositionC
    '[ UnwrapTopC PooledConc,
       UnliftToFinalC IO
     ]

type PooledConcToUnliftIOC = UnwrapC PooledConc

pooledConcToIO ::
  ( Carrier m,
    MonadBaseControlPure IO m
  ) =>
  PooledConcToIOC m a ->
  m a
pooledConcToIO =
  unliftToFinal
    .# unwrapTop
    .# runComposition
{-# INLINE pooledConcToIO #-}

pooledConcToUnliftIO ::
  Eff (Unlift IO) m =>
  PooledConcToUnliftIOC m a ->
  m a
pooledConcToUnliftIO = unwrap
{-# INLINE pooledConcToUnliftIO #-}

async :: Eff PooledConc m => TaskGroup -> m a -> m (Async a)
async g m = unliftConc $ \lower -> A.async g (lower m)
{-# INLINE async #-}

withAsync :: Eff PooledConc m => TaskGroup -> m a -> (Async a -> m b) -> m b
withAsync g m k = unliftConc $ \lower -> A.withAsync g (lower m) (lower . k)
{-# INLINE withAsync #-}

wait :: Eff PooledConc m => Async a -> m a
wait a = unliftConc $ \_ -> A.wait a
{-# INLINE wait #-}

withTaskGroup :: Eff PooledConc m => Int -> (TaskGroup -> m a) -> m a
withTaskGroup i k = unliftConc $ \lower -> A.withTaskGroup i $ lower . k
{-# INLINE withTaskGroup #-}
