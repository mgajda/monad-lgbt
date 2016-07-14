{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.State.LGBT( LGBT
                               , LGCT
                               , LGST (..)
                               , runLGBT
                               , runLGCT
                               , withGlobal, withLocal
                               , getsLocal,  getsGlobal
                               ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State.Strict

newtype LGBT localState globalState result m a = LGBT { _unLGBT ::
    StateT localState (BacktraceT (Maybe (result, localState)) (StateT globalState m)) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

-- | Local/global state transformer with unlimited continuations @MonadCont@
newtype LGCT localState globalState result m a = LGCT { _unLGCT ::
    StateT localState (ContT (result, localState) (StateT globalState m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

instance MonadTrans (LGBT localState globalState result) where
  lift = LGBT . lift . lift . lift

instance MonadTrans (LGCT localState globalState result) where
  lift = LGCT . lift . lift . lift

-- | Local/global state transformer class abstracts over details of how global
--   and local state are realized.
class Monad m
  =>  LGST m localState globalState
  | m -> localState,
    m -> globalState where
  getLocal  :: m localState
  getGlobal :: m globalState

  putLocal  :: localState  -> m ()
  putGlobal :: globalState -> m ()

  modifyLocal  :: (localState  ->   localState ) -> m ()
  modifyGlobal :: (globalState ->   globalState) -> m ()

getsLocal   :: forall m localState globalState a.
               LGST   m localState globalState
            => (localState  -> a) -> m a
getsLocal  f = f <$> getLocal

getsGlobal  :: forall m localState globalState a.
               LGST   m localState globalState
            => (globalState -> a) -> m a
getsGlobal f = f <$> getGlobal

instance Monad m
      => LGST (LGBT localState globalState result m)
                    localState globalState           where
  getLocal     = LGBT                 get
  getGlobal    = LGBT $ lift $ lift   get
  putLocal     = LGBT .               put
  putGlobal    = LGBT . lift . lift . put
  modifyLocal  = LGBT .               modify
  modifyGlobal = LGBT . lift . lift . modify

-- * These are not instance methods, since liftings need to be explicitly determined.
withLocal  :: Monad m
           => (localState -> m localState)
           -> LGBT localState globalState result m ()
withLocal f = getLocal >>= (lift . f) >>= putLocal

withGlobal  :: Monad m
            => (globalState -> m globalState)
            -> LGBT globalState globalState result m ()
withGlobal f = getGlobal >>= (lift . f) >>= putGlobal

newtype BacktraceT r m a = BacktraceT { runBacktraceT ::       m r  -- ^ failure
                                                      -> (a -> m r) -- ^ success
                                                      ->       m r  -- ^ result
                                      }

instance Functor (BacktraceT r m) where
    fmap f m = BacktraceT $ \cf cs -> runBacktraceT m cf $ cs . f
    {-# INLINE fmap #-}

instance Applicative (BacktraceT r m) where
    pure x  = BacktraceT  (\_cf cs -> cs x)
    {-# INLINE pure #-}
    f <*> v = BacktraceT $ \cf cs -> runBacktraceT f cf
                         $ \r     -> runBacktraceT v cf (cs . r)
    {-# INLINE (<*>) #-}

instance Monad (BacktraceT r m) where
    m >>= k  = BacktraceT $ \cf cs -> runBacktraceT m cf (\v -> runBacktraceT (k v) cf cs)

instance MonadTrans (BacktraceT r) where
    lift m = BacktraceT $ \_cf cs -> m >>= cs
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (BacktraceT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance Alternative (BacktraceT r m) where
  empty   = BacktraceT $ \cf _cs -> cf
  {-# INLINE empty #-}
  a <|> b = BacktraceT $ \cf  cs -> runBacktraceT a (runBacktraceT b cf cs) cs
  {-# INLINE (<|>) #-}

instance MonadPlus (BacktraceT r m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

runLGBT :: forall m localState globalState result.
           Monad  m
        => LGBT localState globalState result m result
        ->      localState
        ->                 globalState
        ->                             m (Maybe (result, localState), globalState)
runLGBT (LGBT act) localState globalState =
    runStateT (runBacktraceT (runStateT act localState) onFailure onSuccess) globalState
  where
    onFailure = pure   Nothing
    onSuccess = pure . Just

runLGCT :: forall m localState globalState result.
           Monad  m
        => LGCT localState globalState result m result
        ->      localState
        ->                 globalState
        ->                             m ((result, localState), globalState)
runLGCT (LGCT act) localState globalState =
    runStateT (runContT (runStateT act localState) return) globalState

