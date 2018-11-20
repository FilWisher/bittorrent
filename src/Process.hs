{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}

module Process
    ( Process(..)
    , Forkable(..)
    , runProcess
    , runProcessAsync
    ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import System.Random

newtype Process c a = Process
    { unProcess :: ReaderT c IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader c
        , MonadIO
        , MonadThrow
        , MonadCatch
        )

class HasTVar a env | env -> a where
    getTVar :: env -> TVar a

instance HasTVar s c => MonadState s (Process c) where
    state f = do
        tvar <- getTVar <$> ask
        liftIO . atomically $ do
            st <- readTVar tvar
            let (v, st') = f st
            writeTVar tvar st'
            return v

runProcess :: MonadIO m => c -> Process c a -> m a
runProcess config proc =
    liftIO $ runReaderT (unProcess proc) config

runProcessAsync :: MonadIO m => c -> Process c a -> m (Async a)
runProcessAsync config proc =
    liftIO $ async (runProcess config proc)

forkProcess :: Process c a -> Process c (Async a)
forkProcess proc = do
    conf <- ask
    liftIO $ async (runProcess conf proc)

class Forkable m where
    forkM :: m a -> m (Async a)

instance Forkable (Process c) where
    forkM = forkProcess
