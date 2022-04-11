{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Language.CoreJoin.Syntax.Initial (
  Process,
  Definition,
  Pattern,
  Value,
  Name,
  freeVariables,
) where

import Control.Applicative qualified as Applicative
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import GHC.Stack (HasCallStack)
import Language.CoreJoin.Syntax.Abstract qualified as Syntax.Abstract

data InitialSyntax

instance Syntax.Abstract.CoreJoinSyntax InitialSyntax where
  type ProcessSyntax InitialSyntax = Process
  type DefinitionSyntax InitialSyntax = Definition
  type PatternSyntax InitialSyntax = Pattern
  type ValueSyntax InitialSyntax = Value
  type NameSyntax InitialSyntax = Name

-- | Process
data Process where
  ProcSend :: !Name -> ![Value] -> Process
  ProcLocalDef :: !Definition -> !Process -> Process
  ProcParallel :: !Process -> !Process -> Process
  ProcInert :: Process
  deriving stock (Eq, Ord, Show)

instance Syntax.Abstract.Process InitialSyntax Process where
  {-# INLINE send #-}
  send procVal msgVals = ProcSend procVal (F.toList msgVals)

  {-# INLINE localDef #-}
  localDef DefVoid p = p
  localDef def p = ProcLocalDef def p

  {-# INLINE parallel #-}
  parallel ProcInert b = b
  parallel a ProcInert = a
  parallel a b = ProcParallel a b

  {-# INLINE inert #-}
  inert = ProcInert

-- | Definition
data Definition where
  DefReactionRule :: !Pattern -> !Process -> Definition
  DefComposition :: !Definition -> !Definition -> Definition
  DefVoid :: Definition
  deriving stock (Eq, Ord, Show)

instance Syntax.Abstract.Definition InitialSyntax Definition where
  void = DefVoid

  compose DefVoid b = b
  compose a DefVoid = a
  compose a b = DefComposition a b

  reaction = DefReactionRule

-- | Pattern
data Pattern where
  PatMessage :: !Name -> ![Name] -> Pattern
  PatSynchronization :: !Pattern -> !Pattern -> Pattern
  deriving stock (Eq, Ord, Show)

instance Syntax.Abstract.Pattern InitialSyntax Pattern where
  match procName argNames = PatMessage procName (F.toList argNames)
  syncAll (pat :| more) = checkDuplicates $ foldr PatSynchronization pat more
   where
    checkDuplicates :: HasCallStack => Pattern -> Pattern
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
data Value where
  ValueLiteral :: !Literal -> Value
  ValueVarLookup :: !Name -> Value
  deriving stock (Eq, Ord, Show)

instance IsString Value where
  fromString = Syntax.Abstract.valueLookup . fromString

-- | Name
newtype Name = Name String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

instance Syntax.Abstract.Name InitialSyntax Name where
  valueLookup = ValueVarLookup

-- | Literal
data Literal where
  LitInteger :: {-# UNPACK #-} !Integer -> Literal
  LitDouble :: {-# UNPACK #-} !Double -> Literal
  deriving stock (Eq, Ord, Show)

instance Syntax.Abstract.Literal InitialSyntax Integer where
  valueLiteral = ValueLiteral . LitInteger

instance Syntax.Abstract.Literal InitialSyntax Double where
  valueLiteral = ValueLiteral . LitDouble

-- Checks

freeVariables :: Process -> Set Name
freeVariables = \case
  ProcSend name values -> Set.insert name $ foldMap valueFreeVars values
  ProcLocalDef definition body ->
    let MkDefinitionVariables
          introducedNames
          freeNamesInDefinition = definitionVars definition
        freeNamesInBody = freeVariables body
     in Set.union
          freeNamesInDefinition
          (Set.difference freeNamesInBody introducedNames)
  ProcParallel proc1 proc2 ->
    Set.union (freeVariables proc1) (freeVariables proc2)
  ProcInert -> Set.empty
 where
  valueFreeVars :: Value -> Set Name
  valueFreeVars = \case
    ValueLiteral ls -> Set.empty
    ValueVarLookup na -> Set.singleton na

data DefinitionVariables = MkDefinitionVariables
  { definitionIntroducedNames :: !(Set Name)
  , definitionFreeNames :: !(Set Name)
  }

definitionVars :: Definition -> DefinitionVariables
definitionVars = \case
  DefReactionRule pattern body ->
    let MkPatternVariables{patternProcessNames, patternItemNames} = patternVariables pattern
        freeVarsInBody = freeVariables body
     in MkDefinitionVariables
          patternProcessNames
          ( Set.difference
              freeVarsInBody
              (patternProcessNames <> patternItemNames)
          )
  DefComposition defA defB ->
    let (MkDefinitionVariables introducedA freeInA) = definitionVars defA
        (MkDefinitionVariables introducedB freeInB) = definitionVars defB
     in MkDefinitionVariables
          (introducedA <> introducedB)
          ( Set.difference freeInA introducedB
              <> Set.difference freeInB introducedA
          )
  DefVoid -> MkDefinitionVariables Set.empty Set.empty

data PatternVariables = MkPatternVariables
  { patternProcessNames :: !(Set Name)
  , patternItemNames :: !(Set Name)
  , patternNamesDefinedMultipleTimes :: !(Set Name)
  }

patternVariables :: Pattern -> PatternVariables
patternVariables = \case
  PatMessage processName messageItems ->
    let multiMessageItemNames = MultiSet.fromList messageItems
        messageItemNames = Set.fromList messageItems
        duplicatesInItemsNames =
          Set.filter (\name -> MultiSet.occur name multiMessageItemNames > 1) messageItemNames
        duplicates =
          duplicatesInItemsNames
            <> if Set.member processName messageItemNames
              then Set.singleton processName
              else Set.empty
     in MkPatternVariables (Set.singleton processName) messageItemNames duplicates
  PatSynchronization patA patB ->
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
