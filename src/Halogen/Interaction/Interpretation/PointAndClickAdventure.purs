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

data StartScene a b = MakeStartScene (Scene a) (a -> b)

derive instance Functor (StartScene a)

data ExistsStartScene b = ExistsStartScene (forall r. ExistsStartSceneK b r -> r)
type ExistsStartSceneK b r = forall a. StartScene a b -> r

instance Functor ExistsStartScene where
  map :: forall b b'. (b -> b') -> ExistsStartScene b -> ExistsStartScene b'
  map f (ExistsStartScene g) = ExistsStartScene g'
    where
    g' :: forall r. ExistsStartSceneK b' r -> r
    g' k = todo "k_ (g_ h_)"
      where
      k_ :: forall a. StartScene a b' -> r
      k_ = k

      g_ :: forall r. ExistsStartSceneK b r -> r
      g_ = g

      h_ :: forall a. StartScene a b -> StartScene a b'
      h_ = map f

mkExistsStartScene :: forall a b. StartScene a b -> ExistsStartScene b
mkExistsStartScene s = ExistsStartScene \k -> k s

runExistsStartScene :: forall b r. ExistsStartSceneK b r -> ExistsStartScene b -> r
runExistsStartScene k1 (ExistsStartScene k2) = k2 k1

data Scene :: Type -> Type
data Scene a = Scene {}

derive instance Functor Scene

--------------------------------------------------------------------------------
-- F
--------------------------------------------------------------------------------

data F :: (Type -> Type) -> Type -> Type
data F m a
  = Prompt String (String -> m a)
  | Print String (Unit -> m a)
  | StartScene (ExistsStartScene (m a))

derive instance Functor m => Functor (F m)

prompt :: forall m. Applicative m => String -> InteractionT F m String
prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

print :: forall m. Applicative m => String -> InteractionT F m Unit
print msg = InteractionT $ liftF $ Interact $ Print msg pure

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
    Console.log $ "Prompt " <> show msg
    spawnWidget wc >>= pure >>> pure
    where
    wc :: WidgetComponent
    wc = mkComponent { initialState, eval, render }
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
          [ HP.classes [ HH.ClassName "widget" ] ]
          ( [ [ HH.div [] [ HH.text msg ] ]
            , [ HH.div [] [ HH.input [ HP.ref inputRefLabel ] ] ]
            , [ HH.div []
                  [ HH.button
                      [ HE.onClick (const (Right unit)) ]
                      [ HH.text "submit" ]
                  ]
              ]
            ] # fold
          )
  Interact (Print msg k) -> do
    Console.log $ "Print " <> show msg
    spawnWidget wc >>= pure >>> pure
    where
    wc :: WidgetComponent
    wc = mkComponent { initialState, eval, render }
      where
      initialState {} = {}

      eval = mkEval defaultEval
        { receive = pure <<< Left
        , handleAction = case _ of
            Left st -> put st
            Right _ -> do
              H.raise $ WidgetOutput $ liftAff $ k unit
        }

      render {} =
        HH.div
          [ HP.classes [ HH.ClassName "widget" ] ]
          [ HH.div [] [ HH.text msg ]
          , HH.div []
              [ HH.button
                  [ HE.onClick (const (Right unit)) ]
                  [ HH.text "next" ]
              ]
          ]
  Interact (StartScene _) -> todo "run.Interact (StartScene _)"

spawnWidget :: WidgetComponent -> HM Unit
spawnWidget wc = do
  modify_ \st -> st { mb_widget = pure wc }

--------------------------------------------------------------------------------
-- widget
--------------------------------------------------------------------------------

type WidgetComponent = Component WidgetQuery WidgetInput WidgetOutput Aff

data WidgetQuery :: forall k. k -> Type
data WidgetQuery a

type WidgetInput = {}

newtype WidgetOutput = WidgetOutput (Aff (Free (InteractionF F Aff) Unit))

type WidgetSlotId = Unit

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
  , mb_widget :: Maybe WidgetComponent
  }

type AppSlots =
  ( widget :: Slot WidgetQuery WidgetOutput WidgetSlotId
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
    , mb_widget: empty
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
  render { mb_widget } =
    HH.div
      [ HP.classes [ HH.ClassName "widgets" ] ]
      ( mb_widget # Array.fromFoldable # map \wc ->
          HH.slot (Proxy :: Proxy "widget") unit wc {} WidgetOutput_AppAction
      )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent { start } =<< HA.awaitBody)

