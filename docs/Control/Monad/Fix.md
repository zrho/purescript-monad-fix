## Module Control.Monad.Fix

#### `fixEffect`

``` purescript
fixEffect :: forall eff a. ((Unit -> a) -> Eff eff a) -> Eff eff a
```

#### `fixPure`

``` purescript
fixPure :: forall a. ((Unit -> a) -> a) -> a
```

#### `MonadFix`

``` purescript
class (Monad m) <= MonadFix m where
  mfix :: forall a. ((Unit -> a) -> m a) -> m a
```

Type class for monads that support fixpoints.

`mfix f` runs `f` once with the eventual result of `f` as input. Make sure
not to apply the supplied function until the computation returned; else
a dynamic error will be thrown.

##### Instances
``` purescript
instance monadFixRWST :: (Monoid w, MonadFix m) => MonadFix (RWST r w s m)
instance monadFixIdentity :: MonadFix Identity
instance monadFixEff :: MonadFix (Eff eff)
instance monadFixFunction :: MonadFix (Function r)
instance monadFixReaderT :: (MonadFix m) => MonadFix (ReaderT r m)
instance monadFixStateT :: (MonadFix m) => MonadFix (StateT s m)
instance monadFixWriterT :: (MonadFix m, Monoid w) => MonadFix (WriterT w m)
```


