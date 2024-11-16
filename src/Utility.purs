module Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "BUG: " <> msg

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg