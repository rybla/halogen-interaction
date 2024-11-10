module Halogen.Interaction.Interaction where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Free as Free
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Rec.Class as MonadRec
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Identity (Identity)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

type Interaction :: ((Type -> Type) -> Type -> Type) -> Type -> Type
type Interaction f = InteractionT f Identity

newtype InteractionT :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
newtype InteractionT f m a = InteractionT (Free (InteractionF f m) a)

derive instance Newtype (InteractionT f m a) _
derive newtype instance Functor (InteractionT f m)
derive newtype instance Apply (InteractionT f m)
derive newtype instance Applicative (InteractionT f m)
derive newtype instance Bind (InteractionT f m)
derive newtype instance Monad (InteractionT f m)
derive newtype instance Semigroup a => Semigroup (InteractionT f m a)
derive newtype instance Monoid a => Monoid (InteractionT f m a)

instance MonadEffect m => MonadEffect (InteractionT f m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (InteractionT f m) where
  liftAff = lift <<< liftAff

instance MonadTrans (InteractionT f) where
  lift = lift

instance MonadRec (InteractionT f m) where
  tailRecM k a = k a >>= case _ of
    MonadRec.Loop x -> MonadRec.tailRecM k x
    MonadRec.Done y -> pure y

instance MonadState state m => MonadState state (InteractionT f m) where
  state = InteractionT <<< liftF <<< Lift <<< state

instance MonadAsk r m => MonadAsk r (InteractionT f m) where
  ask = InteractionT $ liftF $ Lift ask

instance MonadTell w m => MonadTell w (InteractionT f m) where
  tell = InteractionT <<< liftF <<< Lift <<< tell

instance MonadThrow e m => MonadThrow e (InteractionT f m) where
  throwError = InteractionT <<< liftF <<< Lift <<< throwError

lift :: forall f m a. m a -> InteractionT f m a
lift = InteractionT <<< liftF <<< Lift

data InteractionF :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
data InteractionF f m a
  = Lift (m a)
  | Interact (f m a)

derive instance (Functor m, Functor (f m)) => Functor (InteractionF f m)

runInteractionT
  :: forall f m m' a
   . MonadRec m'
  => (forall b. m b -> m' b)
  -> (forall b. f m b -> m' b)
  -> InteractionT f m a
  -> m' a -- m a
runInteractionT k_lift k_interact m = m # Newtype.unwrap # Free.foldFree case _ of
  Lift ma -> k_lift ma
  Interact f -> k_interact f

