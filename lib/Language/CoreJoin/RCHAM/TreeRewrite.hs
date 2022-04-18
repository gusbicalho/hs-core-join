{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.CoreJoin.RCHAM.TreeRewrite where

import Control.Applicative qualified as Applicative
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.Writer.CPS (Writer, WriterT)
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Functor.Identity qualified as Identity
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Debug.Trace qualified as Trace
import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial
import Language.CoreJoin.Syntax.Sugar qualified as S
import Utils.Transformers qualified as Utils.Trans

ex :: Syntax.Initial.Process String
ex =
  S.def
    []
    [ "output" S.|<< [S.litI 2]
    , "output" S.|<< [S.litI 3]
    ]

eval :: Syntax.Initial.Process String -> ([OutputItem], Maybe ExecutionError)
eval =
  unwrap
    . Identity.runIdentity
    . flip State.evalStateT (MkFresh 0)
    . Writer.runWriterT
    . flip State.evalStateT emptyChem
    . Except.runExceptT
    . runRCHAM
    . rcham
 where
  unwrap (result, output) =
    (DList.toList output, either Just (const Nothing) result)

rcham :: Syntax.Initial.Process String -> RCHAM ()
rcham rawProcess = do
  let initialProcess = MkLocalName 0 <$> rawProcess
  addProcessThread initialProcess
  F.for_
    (Map.toList nativeDefinitions)
    \(nativeName, (pattern, _)) ->
      addDefinitionThread
        (NativeDefinition nativeName (MkLocalName 0 <$> pattern))
  whileM (not . MultiSet.null . processThreads <$> getChem) do
    getChem
      >>= runStep
        [ str_null
        , str_par
        , str_top
        , str_and
        , str_def
        , react
        ]
        (halt "Deadlock!")
 where
  str_null ChemSol{processThreads} =
    processThreads & multiSetMapMaybeToList \case
      p@Syntax.Initial.ProcInert ->
        Just do
          removeProcessThread p
      _ -> Nothing
  str_par ChemSol{processThreads} =
    processThreads & multiSetMapMaybeToList \case
      p@(Syntax.Initial.ProcParallel p1 p2) ->
        Just do
          removeProcessThread p
          addProcessThread p1
          addProcessThread p2
      _ -> Nothing
  str_top ChemSol{definitionThreads} =
    definitionThreads & multiSetMapMaybeToList \case
      d@(DefinitionAST Syntax.Initial.DefVoid) ->
        Just do
          removeDefinitionThread d
      _ -> Nothing
  str_and ChemSol{definitionThreads} =
    definitionThreads & multiSetMapMaybeToList \case
      d@(DefinitionAST (Syntax.Initial.DefComposition d1 d2)) ->
        Just do
          removeDefinitionThread d
          addDefinitionThread (DefinitionAST d1)
          addDefinitionThread (DefinitionAST d2)
      _ -> Nothing
  str_def ChemSol{processThreads} =
    processThreads & multiSetMapMaybeToList \case
      p@(Syntax.Initial.ProcLocalDef def body) ->
        Just do
          removeProcessThread p
          (def, body) <- refreshDefinedNames def body
          addProcessThread body
          addDefinitionThread (DefinitionAST def)
      _ -> Nothing
  react :: ChemicalSolution -> [RCHAM ()]
  react ChemSol{definitionThreads, processThreads} =
    let sent =
          processThreads & MultiSet.mapMaybe \case
            p@(Syntax.Initial.ProcSend process args) -> Just (process, args, p)
            _ -> Nothing
     in definitionThreads & multiSetConcatMap \case
          DefinitionAST (Syntax.Initial.DefReactionRule pattern body) ->
            satisfying sent pattern <&> \(match, consumed, _) -> do
              let bindArg = \case
                    Syntax.Initial.ValueVarLookup name
                      | Just value <- Map.lookup name match ->
                        value
                    v -> v
              F.for_ consumed removeProcessThread
              addProcessThread (Syntax.Initial.mapValues bindArg body)
          NativeDefinition nativeName pattern ->
            satisfying sent pattern <&> \(match, consumed, _) -> do
              case Map.lookup nativeName nativeDefinitions of
                Nothing -> halt $ "Unknown native process: " <> nativeName
                Just (_, action) -> do
                  F.for_ consumed removeProcessThread
                  action (Map.mapKeys (fmap nameString) . fmap (fmap nameString) $ match)
          _ -> []
  satisfying consumable = \case
    Syntax.Initial.PatMessage patProcess patMsg ->
      consumable & multiSetMapMaybeToList \p@(sentProcess, sentMsg, wholeProcess) ->
        case sentProcess of
          Syntax.Initial.ValueVarLookup sentProcessName
            | patProcess == sentProcessName && length patMsg == length sentMsg ->
              Just
                ( Map.fromList (zip patMsg sentMsg)
                , MultiSet.singleton wholeProcess
                , MultiSet.delete p consumable
                )
          _ -> Nothing
    Syntax.Initial.PatSynchronization pat1 pat2 -> do
      (match1, consumed1, consumable) <- satisfying consumable pat1
      (match2, consumed2, consumable) <- satisfying consumable pat2
      pure (match1 <> match2, consumed1 <> consumed2, consumable)
  refreshDefinedNames def body = do
    let definedNames = Syntax.Initial.definedNames def
    replacements <- traverse freshName $ Map.fromSet nameString definedNames
    let replaceName oldName = Maybe.fromMaybe oldName (Map.lookup oldName replacements)
    pure (replaceName <$> def, replaceName <$> body)

nativeDefinitions ::
  Map
    String
    ( Syntax.Initial.Pattern String
    , Map
        (Syntax.Initial.Name String)
        (Syntax.Initial.Value String) ->
      RCHAM ()
    )
nativeDefinitions =
  Map.fromList
    [
      ( "output"
      ,
        ( "output" S.|>> ["arg"]
        , \args -> case Map.lookup "arg" args of
            Nothing -> halt "Missing argument for output"
            Just (Syntax.Initial.ValueVarLookup name) ->
              halt $ "output sent a free variable: " <> show name
            Just (Syntax.Initial.ValueLiteral lit) -> case lit of
              Syntax.Initial.LitInteger i -> output (OutputI i)
              Syntax.Initial.LitDouble d -> output (OutputD d)
        )
      )
    ]

multiSetMapMaybeToList :: (a -> Maybe b) -> MultiSet a -> [b]
multiSetMapMaybeToList f =
  concatMap (\(b, occur) -> replicate occur b)
    . Maybe.mapMaybe (\(a, occur) -> f a <&> (,occur))
    . MultiSet.toOccurList

multiSetConcatMap :: (a -> [b]) -> MultiSet a -> [b]
multiSetConcatMap f =
  concat
    . concatMap (\(a, occur) -> replicate occur (f a))
    . MultiSet.toOccurList

runStep :: [input -> [f result]] -> f result -> input -> f result
runStep possibleActions onNoneMatch input =
  case concatMap ($ input) possibleActions of
    [] -> onNoneMatch
    result : _ -> result

whileM :: Monad m => m Bool -> m a -> m ()
whileM condM body =
  let go =
        condM >>= \case
          True -> body *> go
          False -> pure ()
   in go

-- Runtime state

data ChemicalSolution = ChemSol
  { processThreads :: !(MultiSet (Syntax.Initial.Process LocalName))
  , definitionThreads :: !(MultiSet DefinitionThread)
  }
  deriving (Eq, Ord, Show)

type ProcessThread = Syntax.Initial.Process LocalName

data DefinitionThread where
  NativeDefinition :: String -> Syntax.Initial.Pattern LocalName -> DefinitionThread
  DefinitionAST :: Syntax.Initial.Definition LocalName -> DefinitionThread
  deriving (Eq, Ord, Show)

data OutputItem where
  OutputD :: Double -> OutputItem
  OutputI :: Integer -> OutputItem
  deriving (Eq, Ord, Show)

data ExecutionError = MkExecutionError ChemicalSolution String
  deriving (Eq, Ord, Show)

newtype Fresh = MkFresh Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data LocalName = MkLocalName {nameId :: Word64, nameString :: String}
  deriving stock (Eq, Ord, Show)

-- Execution monad

newtype RCHAM a = MkRCHAM
  { runRCHAM ::
      Utils.Trans.Stack
        [ HaltT
        , ChemicalStateT
        , OutputT
        , FreshNameT
        ]
        Identity
        a
  }
  deriving newtype (Functor, Applicative, Monad)

-- Halt

type HaltT = ExceptT ExecutionError

halt :: String -> RCHAM ()
halt msg = do
  chem <- getChem
  haltE (MkExecutionError chem msg)

haltE :: ExecutionError -> RCHAM ()
haltE = MkRCHAM . Except.throwE

-- ChemicalState

type ChemicalStateT = StateT ChemicalSolution

emptyChem :: ChemicalSolution
emptyChem = ChemSol mempty mempty

addProcessThread :: ProcessThread -> RCHAM ()
addProcessThread p = modifyChem \chem ->
  chem{processThreads = MultiSet.insert p (processThreads chem)}

removeProcessThread :: ProcessThread -> RCHAM ()
removeProcessThread p = modifyChem \chem ->
  chem{processThreads = MultiSet.delete p (processThreads chem)}

addDefinitionThread :: DefinitionThread -> RCHAM ()
addDefinitionThread p = modifyChem \chem ->
  chem{definitionThreads = MultiSet.insert p (definitionThreads chem)}

removeDefinitionThread :: DefinitionThread -> RCHAM ()
removeDefinitionThread p = modifyChem \chem ->
  chem{definitionThreads = MultiSet.delete p (definitionThreads chem)}

putChem :: ChemicalSolution -> RCHAM ()
putChem = MkRCHAM . Trans.lift . State.put

getChem :: RCHAM ChemicalSolution
getChem = MkRCHAM . Trans.lift $ State.get

modifyChem :: (ChemicalSolution -> ChemicalSolution) -> RCHAM ()
modifyChem = MkRCHAM . Trans.lift . State.modify'

-- Output

type OutputT = WriterT (DList OutputItem)

output :: OutputItem -> RCHAM ()
output = MkRCHAM . Trans.lift . Trans.lift . Writer.tell . DList.singleton

-- Fresh name

type FreshNameT = StateT Fresh

freshName :: String -> RCHAM LocalName
freshName s = MkRCHAM do
  MkFresh id <- Trans.lift . Trans.lift . Trans.lift $ State.get
  Trans.lift . Trans.lift . Trans.lift . State.put $ MkFresh (id + 1)
  pure MkLocalName{nameId = id, nameString = s}
