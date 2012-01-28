-- This is the land of the orphans, so NO WHINING
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Dangerous.Extensions where
import Prelude hiding ( log )
import Control.Dangerous
import Control.Monad.Cont
import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

instance (Errorable m) => Errorable (ContT s m) where
    log = lift . log
    exit = lift . exit

instance (Errorable m) => Errorable (MaybeT m) where
    log = lift . log
    exit = lift . exit

instance (Monoid s, Errorable m) => Errorable (WriterT s m) where
    log = lift . log
    exit = lift . exit

instance (Errorable m) => Errorable (ReaderT s m) where
    log = lift . log
    exit = lift . exit

instance (Errorable m) => Errorable (StateT s m) where
    log = lift . log
    exit = lift . exit
