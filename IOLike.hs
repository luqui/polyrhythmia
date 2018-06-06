{-# LANGUAGE RankNTypes #-}

module IOLike 
    ( MonadFork(..), forkM_
    , MonadTVars(..)
    , NatMonadTrans(..)
    , STM, atomicallyT, atomically, TVar
    )
where

import Control.Monad (void)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

class (Monad m) => MonadFork m where
    forkM :: m () -> m ThreadId

instance MonadFork IO where
    forkM = forkIO

instance (MonadFork m) => MonadFork (ReaderT r m) where
    forkM m = ReaderT $ forkM . runReaderT m

forkM_ :: (MonadFork m) => m () -> m ()
forkM_ = void . forkM



class (Monad m) => MonadTVars m where
    newVar :: a -> m (TVar a)
    readVar :: TVar a -> m a
    writeVar :: TVar a -> a -> m ()
    modifyVar :: TVar a -> (a -> a) -> m ()

instance MonadTVars IO where
    newVar = newTVarIO
    readVar = atomically . readTVar
    writeVar v = atomically . writeTVar v
    modifyVar v = atomically . modifyTVar v

instance MonadTVars STM where
    newVar = newTVar
    readVar = readTVar
    writeVar = writeTVar
    modifyVar = modifyTVar

instance (MonadTVars m) => MonadTVars (ReaderT r m) where
    newVar = lift . newVar
    readVar = lift . readVar
    writeVar v = lift . writeVar v
    modifyVar v = lift . modifyVar v

instance (MonadTVars m) => MonadTVars (StateT s m) where
    newVar = lift . newVar
    readVar = lift . readVar
    writeVar v = lift . writeVar v
    modifyVar v = lift . modifyVar v


class (MonadTrans t) => NatMonadTrans t where
    liftTrans :: (forall a. m a -> n a) -> (forall a. t m a -> t n a)

instance NatMonadTrans (ReaderT r) where
    liftTrans f t = ReaderT $ \r -> f (runReaderT t r)

instance NatMonadTrans (StateT r) where
    liftTrans f t = StateT $ \s -> f (runStateT t s)
    

atomicallyT :: (NatMonadTrans t) => t STM a -> t IO a
atomicallyT = liftTrans atomically

