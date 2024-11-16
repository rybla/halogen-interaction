module Halogen.Interaction.Interpretation.SimpleWidget where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (get, modify_)
import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List, (:))
import Data.Maybe (Maybe, fromMaybe')
import Data.Newtype as Newtype
import Effect (Effect)
import Effect.Aff (Aff, never)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Halogen (Component, HalogenM, HalogenQ, Slot, ComponentHTML, defaultEval, liftEffect, mkComponent, mkEval, unComponent)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Interaction.Interaction (InteractionF(..), InteractionT(..), runInteractionT)
import Halogen.Query.HalogenM (mapOutput)
import Halogen.VDom.Driver as HVD
import Type.Prelude (Proxy(..))
import Utility (bug, todo)
import Web.HTML.HTMLInputElement as HTMLInputElement

--------------------------------------------------------------------------------
-- F
--------------------------------------------------------------------------------

data F :: (Type -> Type) -> Type -> Type
data F m a
  = Prompt String (String -> m a)
  | Print String (Unit -> m a)

derive instance Functor m => Functor (F m)

prompt :: forall m. Applicative m => String -> InteractionT F m String
prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

print :: forall m. Applicative m => String -> InteractionT F m Unit
print msg = InteractionT $ liftF $ Interact $ Print msg pure

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

type M = HalogenM AppState AppAction AppSlots AppOutput Aff

run :: InteractionT F Aff Unit -> M Unit
run (InteractionT ff) = (ff :: Free (InteractionF F Aff) Unit) # runFreeM case _ of
  Lift ma -> ma # lift
  Interact (Prompt msg k) -> spawnWidget component >>= pure >>> pure
    where
    component :: WidgetComponent _
    component = WidgetComponent $ mkComponent { initialState, eval, render }
      where
      inputRefLabel = H.RefLabel "input"

      initialState {} = {}

      eval = mkEval defaultEval
        { handleAction = const do
            inputElement <- H.getHTMLElementRef inputRefLabel <#> fromMaybe' \_ -> bug "impossible, since input must exist"
            str <- inputElement # HTMLInputElement.fromHTMLElement # fromMaybe' (\_ -> bug "impossible, since input must be an input element") # HTMLInputElement.value # liftEffect
            H.raise $ WidgetOutput $ liftAff $ k str
        }

      render {} =
        HH.div
          [ HP.classes [ HH.ClassName "widget" ] ]
          [ HH.div [] [ HH.text msg ]
          , HH.div [] [ HH.input [ HP.ref inputRefLabel ] ]
          , HH.div []
              [ HH.button
                  [ HE.onClick (const unit) ]
                  [ HH.text "submit" ]
              ]
          ]
  Interact (Print msg k) -> spawnWidget component >>= pure >>> pure
    where
    component :: WidgetComponent _
    component = WidgetComponent $ mkComponent { initialState, eval, render }
      where
      initialState {} = {}

      eval = mkEval defaultEval
        { initialize = pure unit
        , handleAction = const do
            H.raise $ WidgetOutput $ liftAff $ k unit
        }

      render {} =
        HH.div
          [ HP.classes [ HH.ClassName "widget" ] ]
          [ HH.div [] [ HH.text msg ] ]

spawnWidget :: forall a. WidgetComponent a -> M Unit
spawnWidget widgetComponent = do
  modify_ \st -> st { widgetComponents = mkExists widgetComponent : st.widgetComponents }

--------------------------------------------------------------------------------
-- widget
--------------------------------------------------------------------------------

data WidgetComponent a = WidgetComponent (Component WidgetQuery WidgetInput (WidgetOutput a) Aff)

mapExistsWidgetOutput :: forall a. WidgetComponent a -> Component WidgetQuery WidgetInput (Exists WidgetOutput) Aff
mapExistsWidgetOutput (WidgetComponent wc) = wc # unComponent f
  where
  f
    :: forall state action slots1
     . { eval :: forall x. HalogenQ WidgetQuery action WidgetInput x -> HalogenM state action slots1 (WidgetOutput a) Aff x
       , initialState :: WidgetInput -> state
       , render :: state -> ComponentHTML action slots1 Aff
       }
    -> _
  f { initialState, eval, render } =
    let
      eval' :: forall y. HalogenQ WidgetQuery action _ y -> HalogenM state action slots1 (Exists WidgetOutput) Aff y
      eval' q = eval q # mapOutput mkExists
    in
      mkComponent { initialState, eval: eval', render }

data WidgetQuery :: forall k. k -> Type
data WidgetQuery a

type WidgetInput = {}

newtype WidgetOutput a = WidgetOutput (InteractionT F Aff a)

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
  , widgetComponents :: List (Exists WidgetComponent)
  }

type AppSlots = (widget :: Slot WidgetQuery (Exists WidgetOutput) WidgetSlotId)

data AppAction
  = Initialize_AppAction
  | WidgetOutput_AppAction (Exists WidgetOutput)
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
    { initialize = pure Initialize_AppAction
    , handleAction = case _ of
        Initialize_AppAction -> do
          { start } <- get
          start # run
        WidgetOutput_AppAction ewo -> do
          Console.log "appComponent.eval.handleAction.WidgetOutput_AppAction"
          ewo # runExists \(WidgetOutput wo) -> run wo >>= \_ -> pure unit
        Noop_AppAction -> do
          Console.log "appComponent.eval.handleAction.Noop_AppAction"
          pure unit
    }
  render { widgetComponents } =
    HH.div
      []
      ( [ widgetComponents # Array.fromFoldable # mapWithIndex \i ewc -> ewc # runExists \wc ->
            HH.slot (Proxy :: Proxy "widget") unit (wc # mapExistsWidgetOutput) {}
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

