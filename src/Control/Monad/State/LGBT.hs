{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.State.LGBT( LGBT
                               , LGLT
                               , LGCT
                               , MonadLGBT (..)
                               , runLGBT
                               , runLGLT
                               , runLGCT
                               , withGlobal, withLocal
                               , getsLocal,  getsGlobal
                               ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Logic
--import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Backtrack

-- | LGBT monad is sandwiching state with eager @BacktrackT@ transformer.
newtype LGBT localState globalState m a = LGBT { _unLGBT :: forall result.
    StateT                             localState
   (BacktrackT (Either String (result, localState))
   (StateT                                         globalState m)) a }

instance Functor (LGBT localState globalState m) where
  fmap f (LGBT act) = LGBT $ fmap f act

instance Applicative (LGBT localState globalState m) where
  LGBT f <*> LGBT a = LGBT $ f <*> a
  pure v            = LGBT $ pure v

instance Applicative m
      => Alternative (LGBT localState globalState m) where
  empty             = LGBT empty
  LGBT a <|> LGBT b = LGBT $ a <|> b
  some (LGBT act)   = LGBT $ some act
  many (LGBT act)   = LGBT $ many act

instance Monad                              m
      => Monad (LGBT localState globalState m) where
  return v     = LGBT $ return v
  LGBT a >>= b = LGBT $ a >>= (_unLGBT . b)
  fail   s     = LGBT $ fail s

instance MonadPlus                              m
      => MonadPlus (LGBT localState globalState m) where
  mzero = LGBT mzero
  LGBT a `mplus` LGBT b = LGBT $ a `mplus` b

instance MonadIO                              m
      => MonadIO (LGBT localState globalState m) where
  liftIO act = LGBT $ liftIO act

-- | LGBT monad is sandwiching state with fair @LogicT@ transformer.
newtype LGLT localState globalState m a =
    LGLT { _unLGLT ::
             StateT localState (LogicT (StateT globalState m)) a }
  deriving (Functor, Applicative, Alternative,
            Monad, MonadPlus, MonadIO, MonadLogic)

{- TODO: MonadRead and MonadExcept instances:
instance MonadReader r           m
      => MonadReader r (LGLT l g m)
  where
    ask = lift . lift . lift $ ask
    --local f m = LGLT $ local f (_unLGLT m ((local f .) . sk (local f fk))
    -- \sk fk -> unLogicT m ((local f .) . sk) (local f fk)

instance MonadError e           m
      => MonadError e (LGLT l g m)
  where
    throwError = lift . throwError
    -- catchError
 -}

-- | Local/global state transformer with unlimited continuations @MonadCont@
newtype LGCT localState globalState result m a = LGCT { _unLGCT ::
    StateT localState (ContT (result, localState) (StateT globalState m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

instance MonadTrans (LGBT localState globalState) where
  lift act = LGBT $ lift $ lift $ lift act

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
  =>  MonadLGBT m localState globalState
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
               MonadLGBT   m localState globalState
            => (localState  -> a) -> m a
getsLocal  f = f <$> getLocal

getsGlobal  :: forall m localState globalState a.
               MonadLGBT   m localState globalState
            => (globalState -> a) -> m a
getsGlobal f = f <$> getGlobal

instance Monad m
      => MonadLGBT (LGBT localState globalState m)
                         localState globalState    where
  getLocal       = LGBT                 get
  getGlobal      = LGBT $ lift $ lift   get
  putLocal     l = LGBT $               put l
  putGlobal    g = LGBT $ lift . lift $ put g
  modifyLocal  m = LGBT $               modify m
  modifyGlobal m = LGBT $ lift . lift $ modify m

instance Monad m
      => MonadLGBT (LGLT localState globalState m)
                   localState globalState    where
  getLocal     = LGLT                 get
  getGlobal    = LGLT $ lift $ lift   get
  putLocal     = LGLT .               put
  putGlobal    = LGLT . lift . lift . put
  modifyLocal  = LGLT .               modify
  modifyGlobal = LGLT . lift . lift . modify

-- * These are not instance methods, since choice of transformer needs
--   to be explicitly determined.
withLocal :: forall (t :: (* -> *) -> * -> *) -- Any transformer that makes MonadLGBT
                          (m ::       * -> *) -- Any underlying monad, below the @t@
                             localState
                                        globalState.
            (Monad        m,
             MonadTrans t  ,
             MonadLGBT (t m) localState      globalState)
          =>                (localState -> m localState )
          -> t m ()
withLocal f = getLocal >>= (lift . f) >>= putLocal

withGlobal :: forall (t :: (* -> *) -> * -> *) -- Any transformer that makes MonadLGBT
                     (m ::  * -> *           ) -- Any underlying monad, below the @t@
                              localState
                                         globalState.
             (Monad        m,
              MonadTrans t  ,
              MonadLGBT (t m) localState globalState)
           =>                           (globalState -> m globalState)
           ->            t m ()
withGlobal f = getGlobal >>= (lift . f) >>= putGlobal

runLGBT :: forall m localState globalState result.
           Monad  m
        => LGBT localState globalState m         result
        ->      localState
        ->                 globalState
        ->                             m (Either String (result, localState), globalState)
runLGBT (LGBT act) localState globalState =
    runStateT (runBacktrackT (runStateT act localState) onFailure onSuccess) globalState
  where
    onFailure = pure . Left
    onSuccess = pure . Right

runLGLT :: forall m  result success localState globalState.
           Monad  m
        => LGLT             localState    globalState    m success
        ->                  localState
        ->                                globalState
        -> (success ->      localState -> globalState -> m result  -> m result)
        -> (                              globalState ->              m result)
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

