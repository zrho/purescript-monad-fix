module Test.Main where

import Prelude

import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Apply
import Data.Identity
import Data.Tuple
import Test.Assert

type Effects eff = (console :: CONSOLE, assert :: ASSERT | eff)

main :: forall eff. Eff (Effects eff) Unit
main = do
  identityTest
  effectTest
  pureStateTest
  effectStateTest

identityTest :: forall eff. Eff (Effects eff) Unit
identityTest = do
  let result = runIdentity $ mfix \da -> pure $ Tuple 0 (fst <<< da)
  assert (fst result == 0)
  assert (snd result unit == 0)

effectTest :: forall eff. Eff (Effects eff) Unit
effectTest = do
  result <- mfix \da -> pure $ Tuple 0 (fst <<< da)
  assert (fst result == 0)
  assert (snd result unit == 0)

pureStateTest :: forall eff. Eff (Effects eff) Unit
pureStateTest = do
  let result = execState (mfix (\da -> put da *> pure 1)) (const 0)
  assert (result unit == 1)

effectStateTest :: forall eff. Eff (Effects eff) Unit
effectStateTest = do
  result <- execStateT (mfix (\da -> put da *> pure 1)) (const 0)
  assert (result unit == 1)
