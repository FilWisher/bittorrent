{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Process
    ( Process(..)
    , Forkable(..)
    , runProcess
    , evalProcess
    , evalProcessAsync
    ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import System.Random

newtype Process c s a = Process
    { unProcess :: ReaderT c (StateT s IO) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState s
        , MonadReader c
        , MonadIO
        , MonadThrow
        , MonadCatch
        )

runProcess :: MonadIO m => c -> s -> Process c s a -> m (a, s)
runProcess config state proc =
    liftIO $ runStateT (runReaderT (unProcess proc) config) state

evalProcess :: MonadIO m => c -> s -> Process c s a -> m a
evalProcess config state proc =
    liftIO $ fst <$> runStateT (runReaderT (unProcess proc) config) state

evalProcessAsync :: MonadIO m => c -> s -> Process c s a -> m (Async a)
evalProcessAsync config state proc =
    liftIO $ async (evalProcess config state proc)

forkProcess :: Process c s a -> Process c s (Async (a, s))
forkProcess proc = do
    conf <- ask
    state <- get
    liftIO $ async (runProcess conf state proc)

class Forkable m where
    forkM :: m a -> m (Async a)

instance Forkable (Process c s) where
    forkM = (fmap fst <$>) . forkProcess
