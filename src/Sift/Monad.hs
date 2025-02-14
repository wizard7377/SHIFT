{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Sift.Monad where

import Control.Monad.Trans as Trans
import Data.Functor.Identity (Identity)
import Control.Monad
data LEnv =
    GenP {}
newtype LMT s m a = LMT { unLMT :: s -> LEnv -> m a }
type LM s a = LMT s Identity a

instance (Monad m, Applicative (LMT s m)) => Monad (LMT s m) where 
  return :: a -> LMT s m a
  return v = LMT $ \_ _ -> return v
  (>>=) :: LMT s m a -> (a -> LMT s m b) -> LMT s m b
  v >>= c = LMT $ \state env -> 
    (let {
        r0 = (unLMT v) state env ;
        r1 = (lift r0) >>= c ;
        r2 = (unLMT r1) state env
    } in r2)
  

instance Trans.MonadTrans (LMT s) where
  lift :: m a -> LMT s m a
  lift inner = LMT $ \_ _ -> inner
