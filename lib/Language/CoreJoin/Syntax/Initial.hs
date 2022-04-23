{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.CoreJoin.Syntax.Initial (
  Process (ProcSend, ProcLocalDef, ProcParallel, ProcInert),
  Definition (DefReactionRule, DefComposition, DefVoid),
  Pattern (PatMessage, PatSynchronization),
  Value (..),
  Literal (..),
  Name (..),
  freeVariables,
  definedNames,
  matchedChannelNames,
  mapValues,
) where

import Control.Applicative qualified as Applicative
import Data.Coerce (coerce)
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import GHC.Stack (HasCallStack)
import Language.CoreJoin.Syntax.Abstract qualified as Syntax.Abstract

data InitialSyntax name

instance
  ( Show name
  , Ord name
  ) =>
  Syntax.Abstract.CoreJoinSyntax (InitialSyntax name)
  where
  type ProcessSyntax (InitialSyntax name) = Process name
  type DefinitionSyntax (InitialSyntax name) = Definition name
  type PatternSyntax (InitialSyntax name) = Pattern name
  type ValueSyntax (InitialSyntax name) = Value name
  type NameSyntax (InitialSyntax name) = Name name

-- | Process
data Process name where
  MkProcSend :: !(Value name) -> ![Value name] -> Process name
  MkProcLocalDef :: !(Definition name) -> !(Process name) -> Process name
  MkProcParallel :: !(Process name) -> !(Process name) -> Process name
  MkProcInert :: Process name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

{-# COMPLETE ProcSend, ProcLocalDef, ProcParallel, ProcInert #-}

pattern ProcSend :: Value name -> [Value name] -> Process name
pattern ProcSend channel args <- MkProcSend channel args
pattern ProcLocalDef :: Definition name -> Process name -> Process name
pattern ProcLocalDef def body <- MkProcLocalDef def body
pattern ProcParallel :: Process name -> Process name -> Process name
pattern ProcParallel proc1 proc2 <- MkProcParallel proc1 proc2
pattern ProcInert :: Process name
pattern ProcInert <- MkProcInert

instance Syntax.Abstract.Process (InitialSyntax name) (Process name) where
  {-# INLINE send #-}
  send procVal msgVals = MkProcSend procVal (F.toList msgVals)

  {-# INLINE localDef #-}
  localDef MkDefVoid p = p
  localDef def p = MkProcLocalDef def p

  {-# INLINE parallel #-}
  parallel MkProcInert b = b
  parallel a MkProcInert = a
  parallel a b = MkProcParallel a b

  {-# INLINE inert #-}
  inert = MkProcInert

-- | Definition
data Definition name where
  MkDefReactionRule :: !(Pattern name) -> !(Process name) -> Definition name
  MkDefComposition :: !(Definition name) -> !(Definition name) -> Definition name
  MkDefVoid :: Definition name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

{-# COMPLETE DefReactionRule, DefComposition, DefVoid #-}

pattern DefReactionRule :: Pattern name -> Process name -> Definition name
pattern DefReactionRule pat reaction <- MkDefReactionRule pat reaction
pattern DefComposition :: Definition name -> Definition name -> Definition name
pattern DefComposition def1 def2 <- MkDefComposition def1 def2
pattern DefVoid :: Definition name
pattern DefVoid <- MkDefVoid

instance Syntax.Abstract.Definition (InitialSyntax name) (Definition name) where
  void = MkDefVoid

  compose MkDefVoid b = b
  compose a MkDefVoid = a
  compose a b = MkDefComposition a b

  reaction = MkDefReactionRule

-- | Pattern
data Pattern name where
  MkPatMessage :: !(Name name) -> ![Name name] -> Pattern name
  MkPatSynchronization :: !(Pattern name) -> !(Pattern name) -> Pattern name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

{-# COMPLETE PatMessage, PatSynchronization #-}

pattern PatMessage :: Name name -> [Name name] -> Pattern name
pattern PatMessage channel args <- MkPatMessage channel args
pattern PatSynchronization :: Pattern name -> Pattern name -> Pattern name
pattern PatSynchronization pat1 pat2 <- MkPatSynchronization pat1 pat2

instance
  (Ord name, Show name) =>
  Syntax.Abstract.Pattern (InitialSyntax name) (Pattern name)
  where
  match procName argNames = MkPatMessage procName (F.toList argNames)
  syncAll (pat :| more) = checkDuplicates $ foldr MkPatSynchronization pat more
   where
    checkDuplicates :: HasCallStack => Pattern name -> Pattern name
    checkDuplicates fullPat =
      case patternVariables fullPat of
        MkPatternVariables{patternNamesDefinedMultipleTimes}
          | null patternNamesDefinedMultipleTimes -> fullPat
          | otherwise ->
            error $
              "Multiple definitions for variables: \n"
                <> show (F.toList patternNamesDefinedMultipleTimes)
                <> "In pattern:\n"
                <> (show fullPat <> "\n")

-- | Value
data Value name where
  ValueLiteral :: !Literal -> Value name
  ValueVarLookup :: !(Name name) -> Value name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsString name => IsString (Value name) where
  fromString = ValueVarLookup . fromString

-- | Name
newtype Name name = MkName {getName :: name}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (IsString)

instance Syntax.Abstract.Name (InitialSyntax name) (Name name) where
  valueLookup = ValueVarLookup

-- | Literal
data Literal where
  LitInteger :: !Integer -> Literal
  LitDouble :: {-# UNPACK #-} !Double -> Literal
  deriving stock (Eq, Ord, Show)

instance (Show name, Ord name) => Syntax.Abstract.Literal (InitialSyntax name) Integer where
  valueLiteral = ValueLiteral . LitInteger

instance (Show name, Ord name) => Syntax.Abstract.Literal (InitialSyntax name) Double where
  valueLiteral = ValueLiteral . LitDouble

-- Useful traversals

mapValues :: (Value name -> Value name) -> Process name -> Process name
mapValues f = goProcess
 where
  goProcess = \case
    p@MkProcInert -> p
    MkProcSend procVal argVals ->
      MkProcSend (f procVal) (f <$> argVals)
    MkProcLocalDef def body ->
      MkProcLocalDef (goDefinition def) (goProcess body)
    MkProcParallel p1 p2 ->
      MkProcParallel (goProcess p1) (goProcess p2)
  goDefinition = \case
    d@MkDefVoid -> d
    MkDefComposition d1 d2 ->
      MkDefComposition (goDefinition d1) (goDefinition d2)
    MkDefReactionRule pat body ->
      MkDefReactionRule pat (goProcess body)

-- Checks

freeVariables :: Ord name => Process name -> Set name
freeVariables = \case
  MkProcSend name values -> foldMap valueFreeVars (name : values)
  MkProcLocalDef definition body ->
    let MkDefinitionVariables
          introducedNames
          freeNamesInDefinition = definitionVars definition
        freeNamesInBody = freeVariables body
     in Set.union
          freeNamesInDefinition
          (Set.difference freeNamesInBody introducedNames)
  MkProcParallel proc1 proc2 ->
    Set.union (freeVariables proc1) (freeVariables proc2)
  MkProcInert -> Set.empty
 where
  valueFreeVars :: Value name -> Set name
  valueFreeVars = \case
    ValueLiteral ls -> Set.empty
    ValueVarLookup (MkName na) -> Set.singleton na

data DefinitionVariables name = MkDefinitionVariables
  { definitionIntroducedNames :: !(Set name)
  , definitionFreeNames :: !(Set name)
  }

definedNames :: Ord name => Definition name -> Set name
definedNames = definitionIntroducedNames . definitionVars

matchedChannelNames :: Ord name => Pattern name -> Set name
matchedChannelNames = patternProcessNames . patternVariables

definitionVars :: Ord name => Definition name -> DefinitionVariables name
definitionVars = \case
  MkDefReactionRule pat body ->
    let MkPatternVariables{patternProcessNames, patternItemNames} = patternVariables pat
        freeVarsInBody = freeVariables body
     in MkDefinitionVariables
          patternProcessNames
          ( Set.difference
              freeVarsInBody
              (patternProcessNames <> patternItemNames)
          )
  MkDefComposition defA defB ->
    let (MkDefinitionVariables introducedA freeInA) = definitionVars defA
        (MkDefinitionVariables introducedB freeInB) = definitionVars defB
     in MkDefinitionVariables
          (introducedA <> introducedB)
          ( Set.difference freeInA introducedB
              <> Set.difference freeInB introducedA
          )
  MkDefVoid -> MkDefinitionVariables Set.empty Set.empty

data PatternVariables name = MkPatternVariables
  { patternProcessNames :: !(Set name)
  , patternItemNames :: !(Set name)
  , patternNamesDefinedMultipleTimes :: !(Set name)
  }

patternVariables :: Ord name => Pattern name -> PatternVariables name
patternVariables = \case
  MkPatMessage (MkName processName) (coerce -> messageItems) ->
    let multiMessageItemNames = MultiSet.fromList messageItems
        messageItemNames = Set.fromList messageItems
        duplicatesInItemsNames =
          Set.filter (\name -> MultiSet.occur name multiMessageItemNames > 1) messageItemNames
        duplicates =
          duplicatesInItemsNames
            <> if Set.member processName messageItemNames
              then Set.singleton processName
              else Set.empty
     in MkPatternVariables
          (Set.singleton processName)
          messageItemNames
          duplicates
  MkPatSynchronization patA patB ->
    let ( MkPatternVariables
            procNamesInA
            messageItemNamesInA
            patternNamesDefinedMultipleTimesA
          ) = patternVariables patA
        ( MkPatternVariables
            procNamesInB
            messageItemNamesInB
            patternNamesDefinedMultipleTimesB
          ) = patternVariables patB
        patternsDefinedMultipleTimes =
          patternNamesDefinedMultipleTimesA
            <> patternNamesDefinedMultipleTimesB
            <> Set.intersection procNamesInA procNamesInB
            <> Set.intersection procNamesInA messageItemNamesInB
            <> Set.intersection procNamesInB messageItemNamesInA
            <> Set.intersection messageItemNamesInA messageItemNamesInB
     in MkPatternVariables
          (procNamesInA <> procNamesInB)
          (messageItemNamesInA <> messageItemNamesInB)
          patternsDefinedMultipleTimes
