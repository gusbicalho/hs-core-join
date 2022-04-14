{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.CoreJoin.Syntax.Abstract (
  CoreJoinSyntax (..),
  Process (..),
  Definition (..),
  Pattern (..),
  Name (..),
  Literal (..),
) where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)

{-
From the paper:

P, Q, R ::=       processes
     x<y~z>       send asynchronous message
  || def D in P   local definition
  || P | Q        parallel composition
  || 0            inert process

D ::=             definitions
     J |> P       reaction rule
  || D /\ D'      composition
  || T            void definition

J ::=             join patterns
     x<y~z>       message pattern
  || J | J'       synchronization
-}

class
  ( Process syntax (ProcessSyntax syntax)
  , Definition syntax (DefinitionSyntax syntax)
  , Pattern syntax (PatternSyntax syntax)
  , Name syntax (NameSyntax syntax)
  ) =>
  CoreJoinSyntax syntax
  where
  type ProcessSyntax syntax = proc | proc -> syntax
  type DefinitionSyntax syntax = def | def -> syntax
  type PatternSyntax syntax = pat | pat -> syntax
  type ValueSyntax syntax = value | value -> syntax
  type NameSyntax syntax = name | name -> syntax

{-
P, Q, R ::=       processes
     x<y~z>       send asynchronous message
  || def D in P   local definition
  || P | Q        parallel composition
  || 0            inert process
-}
class (proc ~ ProcessSyntax syntax) => Process syntax proc where
  send ::
    Foldable list =>
    ValueSyntax syntax ->
    list (ValueSyntax syntax) ->
    proc
  localDef ::
    DefinitionSyntax syntax ->
    proc ->
    proc
  parallel :: proc -> proc -> proc
  inert :: proc

{-
D ::=             definitions
     J |> P       reaction rule
  || D /\ D'      composition
  || T            void definition
-}
class (def ~ DefinitionSyntax syntax) => Definition syntax def where
  reaction ::
    PatternSyntax syntax ->
    ProcessSyntax syntax ->
    def
  compose :: def -> def -> def
  void :: def

{-
J ::=             join patterns
     x<y~z>       message pattern
  || J | J'       synchronization
-}
class (pat ~ PatternSyntax syntax) => Pattern syntax pat where
  {-# MINIMAL match, (sync | syncAll) #-}
  match ::
    Foldable list =>
    NameSyntax syntax ->
    list (NameSyntax syntax) ->
    pat

  -- TODO allow Foldable1 from semigroupoids
  syncAll :: NonEmpty pat -> pat
  syncAll (pat :| morePats) = foldr sync pat morePats
  sync :: pat -> pat -> pat
  sync p1 p2 = syncAll (p1 :| [p2])

-- Names and literals
class (name ~ NameSyntax syntax) => Name syntax name where
  valueLookup :: name -> ValueSyntax syntax

class CoreJoinSyntax syntax => Literal syntax literal where
  valueLiteral :: literal -> ValueSyntax syntax
