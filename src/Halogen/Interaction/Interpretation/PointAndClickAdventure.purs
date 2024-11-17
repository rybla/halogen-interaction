module Halogen.Interaction.Interpretation.PointAndClickAdventure where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe')
import Data.Newtype as Newtype
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen as HHH
import Halogen.Aff as HA
import Halogen.HTML (PlainHTML, fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
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
  = Prompt PlainHTML (String -> m a)
  | ClearPrompt (Unit -> m a)
  | AddConsoleLog PlainHTML (Unit -> m a)
  | SetScene WidgetComponent (Unit -> m a)

derive instance Functor m => Functor (F m)

prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

clearPrompt = InteractionT $ liftF $ Interact $ ClearPrompt pure

addConsoleLog msg = InteractionT $ liftF $ Interact $ AddConsoleLog msg pure

setScene wc = InteractionT $ liftF $ Interact $ SetScene wc pure

--------------------------------------------------------------------------------
-- start
--------------------------------------------------------------------------------

start :: forall m. Applicative m => InteractionT F m Unit
start = do
  name <- prompt $ HH.text "name: "
  addConsoleLog $ HH.text $ "greetings, " <> name
  color <- prompt $ HH.text "favorite color: "
  addConsoleLog $ HH.text $ name <> "'s favorite color is " <> color
  clearPrompt

--------------------------------------------------------------------------------
-- Scene
--------------------------------------------------------------------------------

type SceneSlotId = Int

initialScene :: InteractionT F Aff Unit -> WidgetComponent
initialScene start = mkComponent { initialState, eval, render }
  where
  _effect = Proxy :: Proxy "effect"

  initialState {} = {}

  eval = mkEval defaultEval
    { handleAction = V.match { effect: H.raise <<< WidgetOutput <<< pure <<< Newtype.unwrap } }

  render {} =
    HH.div
      [ HP.classes [ HH.ClassName "Scene" ]
      , HP.style "background-color: lightblue;"
      ]
      [ HH.button
          [ HE.onClick $ const $ V.inj _effect do
              addConsoleLog $ HH.text "clicked 'start'"
              start
              addConsoleLog $ HH.text "setting scene to example scene now ..."
              setScene exampleScene
          ]
          [ HH.text "start" ]
      , HH.button
          [ HE.onClick $ const $ V.inj _effect do
              addConsoleLog (HH.text "clicked 'settings'")
          ]
          [ HH.text "settings" ]
      , HH.button
          [ HE.onClick $ const $ V.inj _effect do
              addConsoleLog (HH.text "clicked 'quite'")
          ]
          [ HH.text "quit" ]
      ]

exampleScene :: WidgetComponent
exampleScene = mkComponent { initialState, eval, render }
  where
  _effect = Proxy :: Proxy "effect"

  initialState {} = {}

  eval = mkEval defaultEval
    { initialize = pure
        ( V.inj _effect do
            void $ prompt $ HH.text $ "what do you think of the new scene?"
            clearPrompt
        )
    , handleAction = V.match { effect: H.raise <<< WidgetOutput <<< pure <<< Newtype.unwrap }
    }

  render {} =
    HH.div
      [ HP.classes [ HH.ClassName "Scene" ]
      , HP.style "background-color: pink;"
      ]
      [ HH.text "this is an example scene" ]

--------------------------------------------------------------------------------
-- promptComponent
--------------------------------------------------------------------------------

promptComponent
  :: PlainHTML
  -> (String -> Aff (Free (InteractionF F Aff) Unit))
  -> WidgetComponent
promptComponent msg k = mkComponent { initialState, eval, render }
  where
  inputRefLabel = H.RefLabel "input"

  initialState {} = {}

  eval = mkEval defaultEval
    { receive = pure <<< Left
    , handleAction = case _ of
        Left st -> put st
        Right _ -> do
          inputElement <- H.getHTMLElementRef inputRefLabel <#> fromMaybe' \_ -> bug "impossible, since input must exist"
          str <- inputElement # HTMLInputElement.fromHTMLElement # fromMaybe' (\_ -> bug "impossible, since input must be an input element") # HTMLInputElement.value # liftEffect
          H.raise $ WidgetOutput $ k str
    }

  render {} =
    HH.div
      [ HP.classes [ HH.ClassName "Widget" ] ]
      ( [ [ HH.div [] [ msg # fromPlainHTML ] ]
        , [ HH.div [] [ HH.input [ HP.ref inputRefLabel ] ] ]
        , [ HH.div []
              [ HH.button
                  [ HE.onClick (const (Right unit)) ]
                  [ HH.text "submit" ]
              ]
          ]
        ] # fold
      )

--------------------------------------------------------------------------------
-- WidgetComponent
--------------------------------------------------------------------------------

type WidgetComponent = Component WidgetQuery WidgetInput WidgetOutput Aff

data WidgetQuery :: forall k. k -> Type
data WidgetQuery a

type WidgetInput = {}

newtype WidgetOutput = WidgetOutput (Aff (Free (InteractionF F Aff) Unit))

--------------------------------------------------------------------------------
-- ConsoleComponent
--------------------------------------------------------------------------------

type ConsoleComponent = Component ConsoleQuery ConsoleInput ConsoleOutput Aff

data ConsoleQuery a =
  AddConsoleLog_ConsoleQuery ConsoleLog a

type ConsoleInput = {}
type ConsoleOutput = Void
type ConsoleSlotId = Unit

consoleComponent :: ConsoleComponent
consoleComponent = mkComponent { initialState, eval, render }
  where
  initialState {} =
    { logs: [] :: Array PlainHTML
    }

  eval = mkEval defaultEval
    { handleQuery = case _ of
        AddConsoleLog_ConsoleQuery cl a -> do
          modify_ \st -> st { logs = st.logs <> [ cl ] }
          pure (pure a)
    }

  render { logs } =
    HHK.div
      [ HP.classes [ HH.ClassName "Console" ] ]
      ( logs # mapWithIndex \i log ->
          show i /\ HH.div [ HP.classes [ HHH.ClassName "ConsoleLog" ] ] [ log # fromPlainHTML ]
      )

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

type HM = HalogenM AppState AppAction AppSlots AppOutput Aff

run :: InteractionT F Aff Unit -> HM Unit
run (InteractionT ff) = (ff :: Free (InteractionF F Aff) Unit) # runFreeM case _ of
  Lift ma -> ma # lift
  Interact (AddConsoleLog msg k) -> do
    H.tell (Proxy :: Proxy "console") unit $ AddConsoleLog_ConsoleQuery $ msg
    k unit # lift
  Interact (Prompt msg k) -> do
    modify_ \st -> st { mb_prompt = pure (promptComponent msg k), prompt_id = st.prompt_id + 1 }
    pure (pure unit)
  Interact (ClearPrompt k) -> do
    modify_ \st -> st { mb_prompt = empty }
    k unit # lift
  Interact (SetScene scene _k) -> do
    modify_ \st -> st { scene = scene, scene_id = st.scene_id + 1 }
    pure (pure unit)

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
  , scene :: WidgetComponent
  , scene_id :: Int
  , mb_prompt :: Maybe WidgetComponent
  , prompt_id :: Int
  }

type ConsoleLog = PlainHTML

type AppSlots =
  ( scene :: Slot WidgetQuery WidgetOutput SceneSlotId
  , prompt :: Slot WidgetQuery WidgetOutput SceneSlotId
  , console :: Slot ConsoleQuery ConsoleOutput ConsoleSlotId
  )

data AppAction
  = WidgetOutput_AppAction WidgetOutput
  | Noop_AppAction

appComponent :: Component AppQuery AppInput AppOutput Aff
appComponent = mkComponent { initialState, eval, render }
  where
  initialState :: AppInput -> AppState
  initialState { start } =
    { start
    , scene: initialScene start
    , scene_id: 0
    , mb_prompt: empty
    , prompt_id: 0
    }
  eval = mkEval defaultEval
    { handleAction = case _ of
        WidgetOutput_AppAction (WidgetOutput wo) -> do
          Console.log "appComponent.eval.handleAction.WidgetOutput_AppAction"
          fiu <- wo # liftAff
          fiu # Newtype.wrap # run
        Noop_AppAction -> do
          Console.log "appComponent.eval.handleAction.Noop_AppAction"
          pure unit
    }
  render { scene, scene_id, mb_prompt, prompt_id } =
    HH.div
      [ HP.classes [ HH.ClassName "App" ] ]
      ( [ [ HH.slot (Proxy :: Proxy "scene") scene_id scene {} WidgetOutput_AppAction ]
        , mb_prompt # Array.fromFoldable # map \wc ->
            HH.slot (Proxy :: Proxy "prompt") prompt_id wc {} WidgetOutput_AppAction
        , [ HH.slot_ (Proxy :: Proxy "console") unit consoleComponent {} ]
        ] # fold
      )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent { start } =<< HA.awaitBody)

