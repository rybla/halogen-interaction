module Halogen.Interaction.Interpretation.PointAndClickAdventure where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Exists (Exists)
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe')
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
import Utility (bug, todo)
import Web.HTML.HTMLInputElement as HTMLInputElement

--------------------------------------------------------------------------------
-- F
--------------------------------------------------------------------------------

data F :: (Type -> Type) -> Type -> Type
data F m a
  = Prompt String (String -> m a)
  | Print String (Unit -> m a)
  | StartScene Scene (Unit -> m a)

derive instance Functor m => Functor (F m)

prompt :: forall m. Applicative m => String -> InteractionT F m String
prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

print :: forall m. Applicative m => String -> InteractionT F m Unit
print msg = InteractionT $ liftF $ Interact $ Print msg pure

--------------------------------------------------------------------------------
-- Scene
--------------------------------------------------------------------------------

data Scene = Scene
  { component :: WidgetComponent
  }

type SceneSlotId = Unit

--------------------------------------------------------------------------------
-- promptComponent
--------------------------------------------------------------------------------

promptComponent :: WidgetComponent
promptComponent = todo "promptComponent"

--------------------------------------------------------------------------------
-- WidgetComponent
--------------------------------------------------------------------------------

type WidgetComponent = Component WidgetQuery WidgetInput WidgetOutput Aff

data WidgetQuery :: forall k. k -> Type
data WidgetQuery a

type WidgetInput = {}

newtype WidgetOutput = WidgetOutput (Aff (Free (InteractionF F Aff) Unit))

--------------------------------------------------------------------------------
-- start
--------------------------------------------------------------------------------

start :: forall m. Applicative m => InteractionT F m Unit
start = do
  name <- prompt "name: "
  print $ "greetings, " <> name
  color <- prompt "favorite color: "
  print $ name <> "'s favorite color is " <> color

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

type HM = HalogenM AppState AppAction AppSlots AppOutput Aff

run :: InteractionT F Aff Unit -> HM Unit
run (InteractionT ff) = (ff :: Free (InteractionF F Aff) Unit) # runFreeM case _ of
  Lift ma -> ma # lift
  Interact (Prompt msg k) -> do
    todo ""
  Interact (Print msg k) -> do
    todo ""
  Interact (StartScene scene k) -> todo "run.Interact (StartScene _)"

startScene :: WidgetComponent -> HM Unit
startScene wc = do
  modify_ \st -> st { mb_scene = pure wc }

--------------------------------------------------------------------------------
-- app
--------------------------------------------------------------------------------

type AppQuery :: Type -> Type
type AppQuery = Const Void

type AppInput =
  { start :: InteractionT F Aff Unit
  }

type AppOutput = {}

type AppState =
  { start :: InteractionT F Aff Unit
  , mb_scene :: Maybe WidgetComponent
  }

type AppSlots =
  ( scene :: Slot WidgetQuery WidgetOutput SceneSlotId
  )

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
    , mb_scene: empty
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
  render { mb_scene } =
    HH.div
      [ HP.classes [ HH.ClassName "scenes" ] ]
      ( mb_scene # Array.fromFoldable # map \wc ->
          HH.slot (Proxy :: Proxy "scene") unit wc {} WidgetOutput_AppAction
      )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent { start } =<< HA.awaitBody)

