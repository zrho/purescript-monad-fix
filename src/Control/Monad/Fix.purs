module Control.Monad.Fix where

import Prelude

import Data.Identity (Identity (..))
import Data.Monoid (class Monoid)
import Data.Tuple (fst)
import Control.Monad.Eff (Eff ())
import Control.Monad.RWS.Trans (RWST (..), RWSResult (..), runRWST)
import Control.Monad.State.Trans (StateT (..), runStateT)
import Control.Monad.Writer.Trans (WriterT (..), runWriterT)
import Control.Monad.Reader.Trans (ReaderT (..), runReaderT)

foreign import fixEffect :: forall eff a. ((Unit -> a) -> Eff eff a) -> Eff eff a
foreign import fixPure :: forall a. ((Unit -> a) -> a) -> a

-- | Type class for monads that support fixpoints.
-- |
-- | `mfix f` runs `f` once with the eventual result of `f` as input. Make sure
-- | not to apply the supplied function until the computation returned; else
-- | a dynamic error will be thrown.
class (Monad m) <= MonadFix m where
  mfix :: forall a. ((Unit -> a) -> m a) -> m a

instance monadFixRWST :: (Monoid w, MonadFix m) => MonadFix (RWST r w s m) where
  mfix f = RWST \r s -> mfix \t -> runRWST (f \u -> case t u of RWSResult _ a _ -> a) r s

-- Pulled in from an old version of purescript-identity
runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

instance monadFixIdentity :: MonadFix Identity where
  mfix = Identity <<< fixPure <<< (\x -> runIdentity <<< x)

instance monadFixEff :: MonadFix (Eff eff) where
  mfix = fixEffect

instance monadFixFunction :: MonadFix (Function r) where
  mfix f r = fixPure (flip f r)

instance monadFixReaderT :: (MonadFix m) => MonadFix (ReaderT r m) where
  mfix f = ReaderT \r -> mfix (flip runReaderT r <<< f)

instance monadFixStateT :: (MonadFix m) => MonadFix (StateT s m) where
  mfix f = StateT \s -> mfix (flip runStateT s <<< f <<< (\x -> fst <<< x))

instance monadFixWriterT :: (MonadFix m, Monoid w) => MonadFix (WriterT w m) where
  mfix f = WriterT $ mfix (runWriterT <<< f <<< (\x -> fst <<< x))
