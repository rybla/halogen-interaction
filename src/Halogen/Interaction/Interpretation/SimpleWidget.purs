module Halogen.Interaction.Interpretation.SimpleWidget where

import Prelude

import Control.Monad.Free (liftF)
import Control.Monad.State (modify_)
import Data.Array as Array
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List, (:))
import Data.Maybe (fromMaybe')
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, HalogenM, Slot, defaultEval, liftEffect, mkComponent, mkEval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Interaction.Interaction (InteractionF(..), InteractionT(..), runInteractionT)
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (Proxy(..))
import Web.HTML.HTMLInputElement as HTMLInputElement

--------------------------------------------------------------------------------
-- F
--------------------------------------------------------------------------------

data F :: (Type -> Type) -> Type -> Type
data F m a
  = Prompt String (String -> m a)
  | Print String (Unit -> m a)

prompt :: forall m. Applicative m => String -> InteractionT F m String
prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

print :: forall m. Applicative m => String -> InteractionT F m Unit
print msg = InteractionT $ liftF $ Interact $ Print msg pure

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

type M = HalogenM AppState AppAction AppSlots AppOutput Aff

run :: forall a. InteractionT F Aff a -> M a
run = runInteractionT liftAff case _ of
  Prompt msg k -> spawnWidget component
    where
    component :: WidgetComponent
    component = mkComponent { initialState, eval, render }
      where
      inputRefLabel = H.RefLabel "input"

      initialState {} = {}

      eval = mkEval defaultEval
        { handleAction = const do
            inputElement <- H.getHTMLElementRef inputRefLabel <#> fromMaybe' \_ -> unsafeCrashWith "impossible"
            str <- inputElement # HTMLInputElement.fromHTMLElement # fromMaybe' (\_ -> unsafeCrashWith "impossible") # HTMLInputElement.value # liftEffect
            H.raise $ WidgetOutput $ liftAff $ (unsafeCrashWith "k") str
        }

      render {} =
        HH.div
          []
          [ HH.div [] [ HH.text msg ]
          , HH.div [] [ HH.input [ HP.ref inputRefLabel ] ]
          , HH.div []
              [ HH.button
                  [ HE.onClick (const unit) ]
                  [ HH.text "submit" ]
              ]
          ]

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

data WidgetQuery :: forall k. k -> Type
data WidgetQuery a

type WidgetInput = {}

newtype WidgetOutput = WidgetOutput (InteractionT F Aff Unit)

unwrapWidgetOutput :: WidgetOutput -> InteractionT F Aff Unit
unwrapWidgetOutput (WidgetOutput i) = i

type WidgetSlotId = Unit

--------------------------------------------------------------------------------
-- app
--------------------------------------------------------------------------------

data AppQuery :: forall k. k -> Type
data AppQuery a

type AppInput =
  { start :: InteractionT F Aff Unit
  }

type AppOutput = {}

type AppState =
  { start :: InteractionT F Aff Unit
  , widgetComponents :: List WidgetComponent
  }

type AppSlots = (widget :: Slot WidgetQuery WidgetOutput WidgetSlotId)

data AppAction
  = WidgetOutput_AppAction WidgetOutput
  | Noop_AppAction

appComponent :: Component AppQuery AppInput AppOutput Aff
appComponent = mkComponent { initialState, eval, render }
  where
  initialState :: AppInput -> AppState
  initialState { start } =
    { start
    , widgetComponents: mempty
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
main = HA.runHalogenAff (HVD.runUI appComponent { start } =<< HA.awaitBody)
  where
  start = do
    name <- prompt "name: "
    print $ "greetings, " <> name

