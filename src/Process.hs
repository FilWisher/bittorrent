{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Process
    ( Process(..)
    , runProcess
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

runProcess :: c -> s -> Process c s a -> IO (a, s)
runProcess config state proc =
    runStateT (runReaderT (unProcess proc) config) state

forkProcess :: Process c s a -> Process c s (Async (a, s))
forkProcess proc = do
    conf <- ask
    state <- get
    liftIO $ async (runProcess conf state proc)
