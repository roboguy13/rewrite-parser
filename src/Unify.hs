{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unify
  where

import           Data.Kind
import           Data.Void
import           Data.Proxy

import           Control.Monad.State
import           Control.Arrow (first)

data UnifyPart m a
  = UnifyVar a
  | UnifyTerminal (UnifyTerm m)
  | UnifyNode [m a]

newtype UnifyEnv m a = UnifyEnv [(a, m Void)]

deriving instance (Show a, Show (m Void)) => Show (UnifyEnv m a)

emptyUnifyEnv :: UnifyEnv m a
emptyUnifyEnv = UnifyEnv []

extendUnifyEnv :: a -> m Void -> UnifyEnv m a -> UnifyEnv m a
extendUnifyEnv v x (UnifyEnv env) = UnifyEnv ((v,x) : env)

unifyEnvLookup :: Eq a => a -> UnifyEnv m a -> Maybe (m Void)
unifyEnvLookup v (UnifyEnv env) = lookup v env

class Eq (UnifyTerm m) => Unify (m :: Type -> Type) where
  type UnifyTerm m

  unifySubst :: Eq a => a -> m Void -> m a -> m a

  getUnifyPart :: m a -> UnifyPart m a

  fromUnifyPart :: UnifyPart m a -> m a

strength1 :: Functor f => (f a, b) -> f (a, b)
strength1 (fa, b) = fmap (,b) fa

match :: forall m a. (Eq a, Unify m) => m a -> m Void -> Maybe (m Void, UnifyEnv m a)
match x0 y0 = strength1 $ runState (go (x0) (y0)) emptyUnifyEnv
  where
    go :: m a -> m Void -> State (UnifyEnv m a) (Maybe (m Void))
    go x y =
      case (getUnifyPart x, getUnifyPart y) of
        (UnifyTerminal tX, UnifyTerminal tY) ->
          if tX == tY
            then pure $ Just y
            else pure Nothing

        (UnifyNode partsX, UnifyNode partsY) ->
          fmap (fmap fromUnifyPart) $ fmap (fmap (UnifyNode) . sequence) $ sequence $ zipWith go partsX partsY

        (UnifyVar v, _) -> do
          fmap (unifyEnvLookup v) get >>= \case
            Just val -> return $ fmap fst $ match val y
            Nothing -> do
              modify (extendUnifyEnv v y)
              pure $ Just y

        (_, _) -> pure Nothing

