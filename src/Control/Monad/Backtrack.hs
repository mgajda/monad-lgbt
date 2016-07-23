{-# LANGUAGE RankNTypes #-}
-- | Simple monad that allows _depth-first_ backtracking
--   instead of fair conjunction/disjunction behaviour
--   as in @LogicT@.
module Control.Monad.Backtrack(
    BacktrackT
  , runBacktrackT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

newtype BacktrackT r m a = BacktrackT {
  runBacktrackT :: (String -> m r) -- failure
                -> (a      -> m r) -- success
                ->            m r  -- result
                }

instance Functor (BacktrackT r m) where
    fmap f m = BacktrackT $ \cf cs -> runBacktrackT m cf $ cs . f
    {-# INLINE fmap #-}

instance Applicative (BacktrackT r m) where
    pure x  = BacktrackT  (\_cf cs -> cs x)
    {-# INLINE pure #-}
    f <*> v = BacktrackT $ \cf cs -> runBacktrackT f cf
                         $ \r     -> runBacktrackT v cf (cs . r)
    {-# INLINE (<*>) #-}

instance Monad (BacktrackT r m) where
    m >>= k  = BacktrackT $ \cf cs -> runBacktrackT m cf (\v -> runBacktrackT (k v) cf cs)
    fail s   = BacktrackT $ \cf _cs -> cf s

instance MonadTrans (BacktrackT r) where
    lift m = BacktrackT $ \_cf cs -> m >>= cs
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (BacktrackT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance Alternative (BacktrackT r m) where
  empty   = BacktrackT $ \cf _cs -> cf "<empty alternative>"
  {-# INLINE empty #-}
  a <|> b = BacktrackT $ \cf  cs -> runBacktrackT a (\_s -> runBacktrackT b cf cs) cs
  {-# INLINE (<|>) #-}
  many = munch []
  {-# INLINE many #-}
  some p = p >>= (\a -> munch [a] p)
  {-# INLINE some #-}

-- | Munch as many as possible, depth-first.
--   Note that it always succeeds - possibly with empty result.
--   That allows it to backjump efficiently, instead of using @Alternative@.
munch :: [a] -> BacktrackT r m a -> BacktrackT r m [a]
munch initialAcc p = BacktrackT $ \_cf cs -> go cs initialAcc
  where
    go cs acc = runBacktrackT p onFailure onSuccess 
      where
        onSuccess a = go cs $ a:acc
        onFailure _ = cs $ reverse acc
{-# INLINE munch #-}

instance MonadPlus (BacktrackT r m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

