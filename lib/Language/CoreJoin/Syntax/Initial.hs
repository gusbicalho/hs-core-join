{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.CoreJoin.Syntax.Initial (
  Process (..),
  Definition (..),
  Pattern (..),
  Value (..),
  Literal (..),
  Name (..),
  freeVariables,
  definedNames,
  mapValues,
) where

import Control.Applicative qualified as Applicative
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
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
  ProcSend :: !(Value name) -> ![Value name] -> Process name
  ProcLocalDef :: !(Definition name) -> !(Process name) -> Process name
  ProcParallel :: !(Process name) -> !(Process name) -> Process name
  ProcInert :: Process name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Syntax.Abstract.Process (InitialSyntax name) (Process name) where
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
data Definition name where
  DefReactionRule :: !(Pattern name) -> !(Process name) -> Definition name
  DefComposition :: !(Definition name) -> !(Definition name) -> Definition name
  DefVoid :: Definition name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Syntax.Abstract.Definition (InitialSyntax name) (Definition name) where
  void = DefVoid

  compose DefVoid b = b
  compose a DefVoid = a
  compose a b = DefComposition a b

  reaction = DefReactionRule

-- | Pattern
data Pattern name where
  PatMessage :: !(Name name) -> ![Name name] -> Pattern name
  PatSynchronization :: !(Pattern name) -> !(Pattern name) -> Pattern name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance
  (Ord name, Show name) =>
  Syntax.Abstract.Pattern (InitialSyntax name) (Pattern name)
  where
  match procName argNames = PatMessage procName (F.toList argNames)
  syncAll (pat :| more) = checkDuplicates $ foldr PatSynchronization pat more
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

instance
  (IsLabel label (Name name)) =>
  IsLabel label (Value name)
  where
  fromLabel = ValueVarLookup $ fromLabel @label

-- | Name
newtype Name name = MkName {getName :: name}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (IsString)

instance
  (IsString name, KnownSymbol label) =>
  IsLabel label (Name name)
  where
  fromLabel = MkName . fromString . symbolVal $ Proxy @label

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
    p@ProcInert -> p
    ProcSend procVal argVals ->
      ProcSend (f procVal) (f <$> argVals)
    ProcLocalDef def body ->
      ProcLocalDef (goDefinition def) (goProcess body)
    ProcParallel p1 p2 ->
      ProcParallel (goProcess p1) (goProcess p2)
  goDefinition = \case
    d@DefVoid -> d
    DefComposition d1 d2 ->
      DefComposition (goDefinition d1) (goDefinition d2)
    DefReactionRule pattern body ->
      DefReactionRule pattern (goProcess body)

-- Checks

freeVariables :: Ord name => Process name -> Set name
freeVariables = \case
  ProcSend name values -> foldMap valueFreeVars (name : values)
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

definitionVars :: Ord name => Definition name -> DefinitionVariables name
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

data PatternVariables name = MkPatternVariables
  { patternProcessNames :: !(Set name)
  , patternItemNames :: !(Set name)
  , patternNamesDefinedMultipleTimes :: !(Set name)
  }

patternVariables :: Ord name => Pattern name -> PatternVariables name
patternVariables = \case
  PatMessage (MkName processName) (coerce -> messageItems) ->
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
