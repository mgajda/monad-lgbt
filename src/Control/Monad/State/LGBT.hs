{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.State.LGBT( LGLT
                               , LGCT
                               , LGBT (..)
                               , runLGLT
                               , runLGCT
                               , withGlobal, withLocal
                               , getsLocal,  getsGlobal
                               ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.Logic
import Control.Monad.State.Strict

newtype LGLT localState globalState m a =
    LGLT { _unLGLT ::
             StateT localState (LogicT (StateT globalState m)) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

-- | Local/global state transformer with unlimited continuations @MonadCont@
newtype LGCT localState globalState result m a = LGCT { _unLGCT ::
    StateT localState (ContT (result, localState) (StateT globalState m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

instance MonadTrans (LGLT localState globalState) where
  lift = LGLT . lift . lift . lift

instance MonadTrans (LGCT localState globalState result) where
  lift = LGCT . lift . lift . lift

-- | Local/global state transformer class abstracts over details of how global
--   and local state are realized.
--   The separation of local and global state only makes sense when we also
--   allow for some backtracking monad in between,
--   hence the name "Local/global backtracking transformer".
class Monad m
  =>  LGBT m localState globalState
  | m -> localState,
    m -> globalState where
  getLocal  :: m localState
  getGlobal :: m globalState

  putLocal  :: localState  -> m ()
  putGlobal :: globalState -> m ()

  modifyLocal  :: (localState  ->   localState ) -> m ()
  modifyLocal m = putLocal . m =<< getLocal

  modifyGlobal :: (globalState ->   globalState) -> m ()
  modifyGlobal m = putGlobal . m =<< getGlobal
  {-# MINIMAL getLocal, getGlobal, putLocal, putGlobal #-}

getsLocal   :: forall m localState globalState a.
               LGBT   m localState globalState
            => (localState  -> a) -> m a
getsLocal  f = f <$> getLocal

getsGlobal  :: forall m localState globalState a.
               LGBT   m localState globalState
            => (globalState -> a) -> m a
getsGlobal f = f <$> getGlobal

instance Monad m
      => LGBT (LGLT localState globalState m)
                    localState globalState    where
  getLocal     = LGLT                 get
  getGlobal    = LGLT $ lift $ lift   get
  putLocal     = LGLT .               put
  putGlobal    = LGLT . lift . lift . put
  modifyLocal  = LGLT .               modify
  modifyGlobal = LGLT . lift . lift . modify

-- * These are not instance methods, since liftings need to be explicitly determined.
withLocal  :: Monad m
           => (localState -> m localState)
           -> LGLT localState globalState m ()
withLocal f = getLocal >>= (lift . f) >>= putLocal

withGlobal  :: Monad m
            =>     (globalState -> m globalState)
            -> LGLT globalState globalState m ()
withGlobal f = getGlobal >>= (lift . f) >>= putGlobal

runLGLT :: forall m localState globalState success result.
           Monad  m
        => LGLT        localState    globalState    m success
        ->             localState
        ->                           globalState
        -> (success -> localState -> globalState -> m result  -> m result)
        -> (                         globalState ->              m result)
        ->                                          m result
runLGLT (LGLT act) localState globalState onSuccess onFailure =
    evalStateT  (runLogicT (runStateT act localState) onSuccess' onFailure') globalState
  where
    onFailure'            = lift . onFailure =<< get
    onSuccess' (r, local) next = do
      global <- get
      lift  $ onSuccess r local global $ evalStateT next global

runLGCT :: forall m localState globalState result.
           Monad  m
        => LGCT localState globalState result m result
        ->      localState
        ->                 globalState
        ->                             m ((result, localState), globalState)
runLGCT (LGCT act) localState globalState =
    runStateT (runContT (runStateT act localState) return) globalState

