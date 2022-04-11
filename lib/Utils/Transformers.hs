{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Transformers where

import Data.Kind (Type)

type family Stack transformers (base :: Type -> Type) where
  Stack '[] base = base
  Stack (trans : more) base = trans (Stack more base)
