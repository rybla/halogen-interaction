module Halogen.Interaction.Interpretation.WidgetList where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List, (:))
import Data.Maybe (fromMaybe')
import Data.Newtype as Newtype
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Halogen (Component, HalogenM, Slot, defaultEval, liftEffect, mkComponent, mkEval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Interaction.Interaction (InteractionF(..), InteractionT(..))
import Halogen.VDom.Driver as HVD
import Type.Prelude (Proxy(..))
import Utility (bug)
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

start :: forall m. Applicative m => InteractionT F m Unit
start = do
  name <- prompt "name: "
  print $ "greetings, " <> name
  color <- prompt "favorite color: "
  print $ name <> "'s favorite color is " <> color

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

type M = HalogenM AppState AppAction AppSlots AppOutput Aff

run :: InteractionT F Aff Unit -> M Unit
run (InteractionT ff) = (ff :: Free (InteractionF F Aff) Unit) # runFreeM case _ of
  Lift ma -> ma # lift
  Interact (Prompt msg k) -> do
    Console.log $ "Prompt " <> show msg
    spawnWidget wc >>= pure >>> pure
    where
    wc :: WidgetComponent
    wc = mkComponent { initialState, eval, render }
      where
      inputRefLabel = H.RefLabel "input"

      initialState { i } = { i }

      eval = mkEval defaultEval
        { receive = pure <<< Left
        , handleAction = case _ of
            Left st -> put st
            Right _ -> do
              inputElement <- H.getHTMLElementRef inputRefLabel <#> fromMaybe' \_ -> bug "impossible, since input must exist"
              str <- inputElement # HTMLInputElement.fromHTMLElement # fromMaybe' (\_ -> bug "impossible, since input must be an input element") # HTMLInputElement.value # liftEffect
              H.raise $ WidgetOutput $ k str
        }

      render { i } =
        HH.div
          [ HP.classes ([ [ HH.ClassName "widget" ], if i == 0 then [ HH.ClassName "active" ] else [] ] # fold) ]
          ( [ [ HH.div [] [ HH.text msg ] ]
            , [ HH.div [] [ HH.input ([ [ HP.ref inputRefLabel ], if i == 0 then [] else [ HP.readOnly true ] ] # fold) ] ]
            , if i == 0 then
                [ HH.div []
                    [ HH.button
                        [ HE.onClick (const (Right unit)) ]
                        [ HH.text "submit" ]
                    ]
                ]
              else []
            ] # fold
          )
  Interact (Print msg k) -> do
    Console.log $ "Print " <> show msg
    spawnWidget wc >>= pure >>> pure
    where
    wc :: WidgetComponent
    wc = mkComponent { initialState, eval, render }
      where
      initialState { i } = { i }

      eval = mkEval defaultEval
        { initialize = pure (Right unit)
        , receive = pure <<< Left
        , handleAction = case _ of
            Left st -> put st
            Right _ -> do
              H.raise $ WidgetOutput $ liftAff $ k unit
        }

      render { i } =
        HH.div
          [ HP.classes ([ [ HH.ClassName "widget" ], if i == 0 then [ HH.ClassName "active" ] else [] ] # fold) ]
          [ HH.div [] [ HH.text msg ] ]

spawnWidget :: WidgetComponent -> M Unit
spawnWidget wc = do
  modify_ \st -> st { widgetComponents = wc : st.widgetComponents }

--------------------------------------------------------------------------------
-- widget
--------------------------------------------------------------------------------

type WidgetComponent = Component WidgetQuery WidgetInput WidgetOutput Aff

data WidgetQuery :: forall k. k -> Type
data WidgetQuery a

type WidgetInput = { i :: Int }

newtype WidgetOutput = WidgetOutput (Aff (Free (InteractionF F Aff) Unit))

type WidgetSlotId = Int

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
  = Initialize_AppAction
  | WidgetOutput_AppAction WidgetOutput
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
        WidgetOutput_AppAction (WidgetOutput wo) -> do
          Console.log "appComponent.eval.handleAction.WidgetOutput_AppAction"
          fiu <- wo # liftAff
          fiu # Newtype.wrap # run
        Noop_AppAction -> do
          Console.log "appComponent.eval.handleAction.Noop_AppAction"
          pure unit
    }
  render { widgetComponents } =
    HH.div
      [ HP.classes [ HH.ClassName "widgets" ] ]
      ( [ widgetComponents # Array.fromFoldable # mapWithIndex \i wc ->
            HH.slot (Proxy :: Proxy "widget") (length widgetComponents - i) wc { i }
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
