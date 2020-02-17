-- | From the PureScript Error.Control package by Luka Jacobowitz.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.ErrorControlInstances () where

import Control.Monad.Except
  (catchError, ExceptT(ExceptT), lift, MonadError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity(Identity), runIdentity)
import Control.Monad.Reader (mapReaderT, ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (mapRWST, runRWST, RWST(RWST))
import Control.Monad.State (mapStateT, runStateT, StateT(StateT))
import Control.Monad.Writer (mapWriterT, runWriterT, WriterT(WriterT))
import Control.Exception (IOException)
import Extra.ErrorControl
import UnexceptionalIO.Trans (run, UIO, unsafeFromIO)

instance ErrorControl IOException IO UIO where
  controlError ma f = unsafeFromIO (ma `catchError` (accept . f))
  accept = run

instance Monad m => ErrorControl e (ExceptT e m) m where
  controlError ma f = runExceptT ma >>= either f pure
  accept = lift

-- | Resolve the error on the left side of an Either.
instance Monad m => ErrorControl (Either e1 e2) (ExceptT (Either e1 e2) m) (ExceptT e2 m) where
  controlError :: ExceptT (Either e1 e2) m a -> (Either e1 e2 -> ExceptT e2 m a) -> ExceptT e2 m a
  controlError ma f =
    ExceptT (pivot <$> runExceptT ma) >>= either pure (f . Left)
    where
      pivot :: Either (Either a b) c -> Either b (Either c a)
      pivot = either (either (Right . Right) Left) (Right . Left)
  accept :: ExceptT e2 m a -> ExceptT (Either e1 e2) m a
  accept = withExceptT Right

#if 0
-- | Resolve the error on the right side of an Either.  (It turns out
-- this actually does conflict with the one above.)
instance Monad m => ErrorControl (Either e1 e2) (ExceptT (Either e1 e2) m) (ExceptT e1 m) where
  controlError ma f =
    ExceptT (pivot <$> runExceptT ma) >>= either (f . Right) pure
    where
      pivot :: Either (Either a b) c -> Either a (Either b c)
      pivot = either (either Left (Right . Left)) (Right . Right)
  accept = withExceptT Left
#endif
