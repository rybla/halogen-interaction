module Halogen.Interaction.Interpretation.Cli where

import Prelude

import Control.Monad.Free (liftF)
import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen.Interaction.Interaction (InteractionF(..), InteractionT(..), runInteractionT)

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

start :: forall m. Applicative m => InteractionT F m Unit
start = do
  name <- prompt "name: "
  print $ "greetings, " <> name
  color <- prompt "favorite color: "
  print $ name <> "'s favorite color is " <> color

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

run :: InteractionT F Aff Unit -> Aff Unit
run = runInteractionT identity case _ of
  Prompt msg k -> console_prompt msg >>= k
  Print msg k -> console_print msg >>= k

--------------------------------------------------------------------------------
-- foreign
--------------------------------------------------------------------------------

foreign import console_prompt_ :: String -> Effect (Promise String)

console_prompt :: String -> Aff String
console_prompt = console_prompt_ >>> Promise.toAffE

foreign import console_print_ :: String -> Effect (Promise Unit)

console_print :: String -> Aff Unit
console_print = console_print_ >>> Promise.toAffE

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = launchAff_ $ run start
