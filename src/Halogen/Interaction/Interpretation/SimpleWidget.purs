module Halogen.Interaction.Interpretation.SimpleWidget where

import Prelude

import Control.Alternative (empty)
import Control.Category (identity)
import Control.Monad.Free (liftF)
import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List, (:))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Interaction.Interaction (InteractionF(..), InteractionT(..), runInteractionT)
import Halogen.Query.HalogenM (mapOutput)
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
-- F
--------------------------------------------------------------------------------

data F :: (Type -> Type) -> Type -> Type
data F m a
  = Prompt String (String -> m a)
  | Print String (Unit -> m a)

prompt :: forall m. Applicative m => String -> InteractionT F m String
prompt msg = InteractionT (liftF (Interact (Prompt msg pure)))

print :: forall m. Applicative m => String -> InteractionT F m Unit
print msg = InteractionT (liftF (Interact (Print msg pure)))

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

type M = HalogenM AppState AppAction AppSlots AppOutput Aff

run :: InteractionT F M Unit -> M Unit
run = runInteractionT identity case _ of
  Prompt msg k -> spawnWidget component
    where
    component :: WidgetComponent
    component = mkComponent { initialState, eval, render }
      where
      initialState = unsafeCrashWith "TODO"
      eval = unsafeCrashWith "TODO"
      render = unsafeCrashWith "TODO"
  Print msg k -> spawnWidget component
    where
    component :: WidgetComponent
    component = mkComponent { initialState, eval, render }
      where
      initialState = unsafeCrashWith "TODO"
      eval = unsafeCrashWith "TODO"
      render = unsafeCrashWith "TODO"

spawnWidget :: forall a. WidgetComponent -> M a
spawnWidget widgetComponent = do
  modify_ \st -> st { widgetComponents = widgetComponent : st.widgetComponents }
  unsafeCrashWith "TODO"

--------------------------------------------------------------------------------
-- widget
--------------------------------------------------------------------------------

type WidgetComponent = Component WidgetQuery WidgetInput WidgetOutput Aff

data WidgetQuery a

type WidgetInput = {}

newtype WidgetOutput = WidgetOutput (InteractionT F M Unit)
unwrapWidgetOutput (WidgetOutput i) = i

type WidgetSlotId = Unit

--------------------------------------------------------------------------------
-- app
--------------------------------------------------------------------------------

data AppQuery a

type AppInput = {}

type AppOutput = {}

type AppState =
  { widgetComponents :: List WidgetComponent
  }

type AppSlots = (widget :: Slot WidgetQuery WidgetOutput WidgetSlotId)

data AppAction
  = WidgetOutput_AppAction WidgetOutput
  | Noop_AppAction

appComponent :: Component AppQuery AppInput AppOutput Aff
appComponent = mkComponent { initialState, eval, render }
  where
  initialState :: AppInput -> AppState
  initialState {} =
    { widgetComponents: mempty
    }
  eval = mkEval defaultEval
    { handleAction = case _ of
        WidgetOutput_AppAction (WidgetOutput wo) -> wo # run
        Noop_AppAction -> pure unit
    }
  render { widgetComponents } =
    HH.div
      []
      ( [ widgetComponents # Array.fromFoldable # mapWithIndex \i widgetComponent ->
            HH.slot (Proxy :: Proxy "widget") unit widgetComponent {}
              if i == 0 then
                WidgetOutput_AppAction
              else
                const Noop_AppAction
        ]
          # fold
      )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent {} =<< HA.awaitBody)

