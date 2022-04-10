{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Functor.Identity qualified as Identity
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty ((:|)))
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

(|>>) ::
  Sugar syntax =>
  Syntax.Abstract.NameSyntax syntax ->
  [Syntax.Abstract.NameSyntax syntax] ->
  Syntax.Abstract.PatternSyntax syntax
p |>> msg = Syntax.Abstract.match p msg
infixr 7 |>>

def ::
  Sugar syntax =>
  [Syntax.Abstract.DefinitionSyntax syntax] ->
  [Syntax.Abstract.ProcessSyntax syntax] ->
  Syntax.Abstract.ProcessSyntax syntax
def defs procs = Syntax.Abstract.localDef (mconcat defs) (mconcat procs)

(|>) ::
  Sugar syntax =>
  NonEmpty (Syntax.Abstract.PatternSyntax syntax) ->
  [Syntax.Abstract.ProcessSyntax syntax] ->
  Syntax.Abstract.DefinitionSyntax syntax
pats |> procs = Syntax.Abstract.reaction (Syntax.Abstract.syncAll pats) (mconcat procs)
infixr 5 |>

(|<<) ::
  Sugar syntax =>
  Syntax.Abstract.ValueSyntax syntax ->
  [Syntax.Abstract.ValueSyntax syntax] ->
  Syntax.Abstract.ProcessSyntax syntax
p |<< msg = Syntax.Abstract.send p msg
infixr 7 |<<

lit ::
  forall literal syntax.
  ( Sugar syntax
  , Syntax.Abstract.Literal syntax literal
  ) =>
  literal ->
  Syntax.Abstract.ValueSyntax syntax
lit = Syntax.Abstract.valueLiteral

litI ::
  ( Sugar syntax
  , Syntax.Abstract.Literal syntax Integer
  ) =>
  Integer ->
  Syntax.Abstract.ValueSyntax syntax
litI = lit

litD ::
  ( Sugar syntax
  , Syntax.Abstract.Literal syntax Double
  ) =>
  Double ->
  Syntax.Abstract.ValueSyntax syntax
litD = lit
