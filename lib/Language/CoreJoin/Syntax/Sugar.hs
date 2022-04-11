{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.CoreJoin.Syntax.Sugar (
  Sugar,
  def,
  (|>>),
  (|<<),
  (|>),
  lit,
  litI,
  litD,
  Syntax.Abstract.Process,
  Syntax.Abstract.Definition,
  Syntax.Abstract.Pattern,
  Syntax.Abstract.Name,
  Syntax.Abstract.Literal,
  Syntax.Abstract.ProcessSyntax,
  Syntax.Abstract.DefinitionSyntax,
  Syntax.Abstract.PatternSyntax,
  Syntax.Abstract.ValueSyntax,
  Syntax.Abstract.NameSyntax,
) where

import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity qualified as Identity
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:)), TypeError)
import Language.CoreJoin.Syntax.Abstract qualified as Syntax.Abstract

class
  ( Syntax.Abstract.CoreJoinSyntax syntax
  , IsString (Syntax.Abstract.NameSyntax syntax)
  , IsString (Syntax.Abstract.ValueSyntax syntax)
  ) =>
  Sugar syntax

instance
  ( Syntax.Abstract.CoreJoinSyntax syntax
  , IsString (Syntax.Abstract.NameSyntax syntax)
  , IsString (Syntax.Abstract.ValueSyntax syntax)
  ) =>
  Sugar syntax

-- "Hidden" instance exists only so that GHC does not unwrap the
-- Sugar constraint during inference.
-- See https://blog.csongor.co.uk/opaque-constraint-synonyms/
data Hidden
instance
  {-# OVERLAPPING #-}
  ( Syntax.Abstract.CoreJoinSyntax Hidden
  , IsString (Syntax.Abstract.NameSyntax Hidden)
  , IsString (Syntax.Abstract.ValueSyntax Hidden)
  ) =>
  Sugar Hidden

{-# INLINE (|>>) #-}
(|>>) ::
  Sugar syntax =>
  Syntax.Abstract.NameSyntax syntax ->
  [Syntax.Abstract.NameSyntax syntax] ->
  Syntax.Abstract.PatternSyntax syntax
p |>> msg = Syntax.Abstract.match p msg
infixr 7 |>>

{-# INLINE def #-}
def ::
  forall syntax.
  Sugar syntax =>
  [Syntax.Abstract.DefinitionSyntax syntax] ->
  [Syntax.Abstract.ProcessSyntax syntax] ->
  Syntax.Abstract.ProcessSyntax syntax
def defs procs =
  Syntax.Abstract.localDef
    (mconcatVia @(DefinitionMonoid syntax) defs)
    (mconcatVia @(ProcessMonoid syntax) procs)

{-# INLINE (|>) #-}
(|>) ::
  forall syntax.
  Sugar syntax =>
  NonEmpty (Syntax.Abstract.PatternSyntax syntax) ->
  [Syntax.Abstract.ProcessSyntax syntax] ->
  Syntax.Abstract.DefinitionSyntax syntax
pats |> procs =
  Syntax.Abstract.reaction
    (sconcatVia @(PatternSemigroup syntax) pats)
    (mconcatVia @(ProcessMonoid syntax) procs)
infixr 5 |>

{-# INLINE (|<<) #-}
(|<<) ::
  Sugar syntax =>
  Syntax.Abstract.NameSyntax syntax ->
  [Syntax.Abstract.ValueSyntax syntax] ->
  Syntax.Abstract.ProcessSyntax syntax
p |<< msg = Syntax.Abstract.send p msg
infixr 7 |<<

{-# INLINE lit #-}
lit ::
  forall literal syntax.
  ( Sugar syntax
  , Syntax.Abstract.Literal syntax literal
  ) =>
  literal ->
  Syntax.Abstract.ValueSyntax syntax
lit = Syntax.Abstract.valueLiteral

{-# INLINE litI #-}
litI ::
  ( Sugar syntax
  , Syntax.Abstract.Literal syntax Integer
  ) =>
  Integer ->
  Syntax.Abstract.ValueSyntax syntax
litI = lit

{-# INLINE litD #-}
litD ::
  ( Sugar syntax
  , Syntax.Abstract.Literal syntax Double
  ) =>
  Double ->
  Syntax.Abstract.ValueSyntax syntax
litD = lit

{- | Helper semigroups/monoids
 | can be used with deriving via or with the helpers below
-}
{-# INLINE mconcatVia #-}
mconcatVia :: forall monoid a. (Coercible a monoid, Monoid monoid) => [a] -> a
mconcatVia = coerce @monoid @a . mconcat . coerce @[a] @[monoid]

{-# INLINE sconcatVia #-}
sconcatVia :: forall semigroup a. (Coercible a semigroup, Semigroup semigroup) => NonEmpty a -> a
sconcatVia = coerce @semigroup @a . sconcat . coerce @(NonEmpty a) @(NonEmpty semigroup)

newtype ProcessMonoid syntax = MkProcessMonoid (Syntax.Abstract.ProcessSyntax syntax)

instance Syntax.Abstract.Process syntax p => Semigroup (ProcessMonoid syntax) where
  {-# INLINE (<>) #-}
  (<>) = coerce (Syntax.Abstract.parallel @syntax)

instance Syntax.Abstract.Process syntax p => Monoid (ProcessMonoid syntax) where
  {-# INLINE mempty #-}
  mempty = coerce (Syntax.Abstract.inert @syntax)

newtype DefinitionMonoid syntax = MkDefinitionMonoid (Syntax.Abstract.DefinitionSyntax syntax)

instance Syntax.Abstract.Definition syntax d => Semigroup (DefinitionMonoid syntax) where
  {-# INLINE (<>) #-}
  (<>) = coerce (Syntax.Abstract.compose @syntax)

instance Syntax.Abstract.Definition syntax d => Monoid (DefinitionMonoid syntax) where
  {-# INLINE mempty #-}
  mempty = coerce (Syntax.Abstract.void @syntax)

newtype PatternSemigroup syntax = MkPatternSemigroup (Syntax.Abstract.PatternSyntax syntax)

instance Syntax.Abstract.Pattern syntax d => Semigroup (PatternSemigroup syntax) where
  {-# INLINE (<>) #-}
  (<>) = coerce (Syntax.Abstract.sync @syntax)
  {-# INLINE sconcat #-}
  sconcat = coerce (Syntax.Abstract.syncAll @syntax)
