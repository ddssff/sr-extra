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

module Extra.ErrorControl
  ( MonadError(throwError) -- re-export
  , ErrorControl(controlError, accept)
  , intercept
  , trial
  , trialT
  , absolve
  , assure
  ) where

import Control.Monad.Except
  (catchError, ExceptT(ExceptT), lift, MonadError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity(Identity), runIdentity)
import Control.Monad.Reader (mapReaderT, ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (mapRWST, runRWST, RWST(RWST))
import Control.Monad.State (mapStateT, runStateT, StateT(StateT))
import Control.Monad.Writer (mapWriterT, runWriterT, WriterT(WriterT))
import Control.Exception (IOException)
import UnexceptionalIO.Trans (run, UIO, unsafeFromIO)

class (MonadError e m, Monad n) => ErrorControl e m n where
  controlError :: m a -> (e -> n a) -> n a
  accept :: n a -> m a

instance ErrorControl e (Either e) Identity where
  controlError ma f = either f Identity ma
  accept = Right . runIdentity

instance ErrorControl e m n => ErrorControl e (StateT s m) (StateT s n) where
  controlError sma f = StateT (\s -> controlError (runStateT sma s) (\e -> runStateT (f e) s))
  accept = mapStateT accept

instance ErrorControl e m n => ErrorControl e (ReaderT r m) (ReaderT r n) where
  controlError rma f = ReaderT (\r -> controlError (runReaderT rma r) (\e -> runReaderT (f e) r))
  accept = mapReaderT accept

instance (ErrorControl e m n, Monoid w) => ErrorControl e (WriterT w m) (WriterT w n) where
  controlError wma f = WriterT (controlError (runWriterT wma) (runWriterT . f))
  accept = mapWriterT accept

instance (ErrorControl e m n, Monoid w) => ErrorControl e (RWST r w s m) (RWST r w s n) where
  controlError rwsma f = RWST (\r s -> controlError (runRWST rwsma r s) (\e -> runRWST (f e) r s))
  accept = mapRWST accept

-- | Enhanced 'handleError'
intercept :: ErrorControl e m n => m a -> (e -> a) -> n a
intercept fa f = controlError fa (pure . f)

-- | Enhanced 'try'
trial :: ErrorControl e m n => m a -> n (Either e a)  -- try
trial fa = intercept (fmap Right fa) Left

trialT :: ErrorControl e m n => m a -> ExceptT e n a
trialT fa = ExceptT (trial fa)

absolve :: ErrorControl e m n => n (Either e a) -> m a
absolve gea = accept gea >>= (either throwError pure)

assure :: ErrorControl e m n => n a -> (a -> e) -> (a -> Bool) -> m a
assure ga err predicate =
  accept ga >>= (\a -> if predicate a then pure a else throwError (err a))
