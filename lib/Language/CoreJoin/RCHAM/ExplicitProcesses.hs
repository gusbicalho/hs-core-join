{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.CoreJoin.RCHAM.ExplicitProcesses where

import Control.Applicative qualified as Applicative
import Control.Monad (forever)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.Writer.CPS (Writer, WriterT)
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Data.Coerce qualified as Coerce
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Functor.Identity qualified as Identity
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Word (Word64)
import Debug.Trace qualified as Trace
import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial
import Language.CoreJoin.Syntax.Sugar qualified as S
import System.Random.Stateful qualified as Random
import Utils.Transformers qualified as Utils.Trans

eval :: Int -> Syntax.Initial.Process String -> ([RTValue], Maybe ExecutionError)
eval seed =
  unwrap
    . Identity.runIdentity
    . flip State.evalStateT (MkFresh 0)
    . Writer.runWriterT
    . flip State.evalStateT emptyChem
    . Except.runExceptT
    . runRCHAM
    . Random.runStateGenT (Random.mkStdGen seed)
    . rcham
 where
  unwrap (result, output) =
    (DList.toList output, either Just (const Nothing) result)

rcham :: Random.RandomGen g => Syntax.Initial.Process String -> Random.StateGenM g -> StateT g RCHAM ()
rcham initialProcess g =
  setup *> run
 where
  setup = Trans.lift do
    env <-
      MkEnv . Map.unions <$> for (Map.toList nativeDefinitions) \(nativeName, (pattern, action)) -> do
        siteId <- freshDefinitionSiteId
        let newChannels = Syntax.Initial.matchedChannelNames pattern
        addDefinitionSite
          siteId
          $ MkDefinitionSite
            { siteDeliveredMessages = MultiSet.empty
            , siteDefinition = Left $ MkNativeDefinition nativeName pattern
            }
        pure $ Map.fromSet (RTChannelReference . MkChannelReference siteId) newChannels
    addProcessThread (MkProcessThread env initialProcess)
  run = do
    chem <- Trans.lift getChem
    let step =
          pickStep
            pick
            [ str_null
            , str_par
            , str_def
            , send
            , deliver
            , react
            ]
            chem
    case step of
      Nothing -> pure ()
      Just step -> do
        step
        run
  pick possibilities = do
    let count = length possibilities
    selectedIndex <- Random.uniformRM (0, count - 1) g
    Trans.lift $ possibilities NonEmpty.!! selectedIndex
  str_null ChemSol{processThreads} =
    processThreads & multiSetMapMaybeToList \case
      p@(MkProcessThread _ Syntax.Initial.ProcInert) -> Just do
        removeProcessThread p
      _ -> Nothing
  str_par ChemSol{processThreads} =
    processThreads & multiSetMapMaybeToList \case
      p@(MkProcessThread env (Syntax.Initial.ProcParallel p1 p2)) -> Just do
        removeProcessThread p
        addProcessThread (MkProcessThread env p1)
        addProcessThread (MkProcessThread env p2)
      _ -> Nothing
  str_def ChemSol{processThreads} =
    processThreads & multiSetMapMaybeToList \case
      p@(MkProcessThread env (Syntax.Initial.ProcLocalDef def body)) ->
        Just do
          removeProcessThread p
          siteId <- freshDefinitionSiteId
          let newChannels = Syntax.Initial.definedNames def
          let newEnv = MkEnv $ Map.union (Map.fromSet (RTChannelReference . MkChannelReference siteId) newChannels) (getEnv env)
          addDefinitionSite
            siteId
            MkDefinitionSite
              { siteDeliveredMessages = mempty
              , siteDefinition =
                  Right $
                    MkIntepretedDefinition
                      { definitionEnv = newEnv
                      , interpretedDefinition = def
                      }
              }
          addProcessThread (MkProcessThread newEnv body)
      _ -> Nothing
  send :: ChemicalSolution -> [RCHAM ()]
  send ChemSol{processThreads, sentMessages} =
    processThreads & multiSetMapMaybeToList \case
      p@(MkProcessThread env (Syntax.Initial.ProcSend process args)) ->
        Just do
          let resolve = \case
                Syntax.Initial.ValueLiteral lit ->
                  pure . RTPrimitive $ case lit of
                    Syntax.Initial.LitInteger n -> PrimInteger n
                    Syntax.Initial.LitDouble x -> PrimDouble x
                Syntax.Initial.ValueVarLookup (Syntax.Initial.MkName name) ->
                  case Map.lookup name (getEnv env) of
                    Nothing -> halt $ "Name not defined: " <> name
                    Just val -> pure val
          reference <-
            resolve process >>= \case
              (RTChannelReference reference) -> pure reference
              other -> halt $ "Invalid send: expected channel, got " <> show other
          argVals <- traverse resolve args
          addSentMessage (MkSentMessage reference argVals)
          removeProcessThread p
      _ -> Nothing
  deliver :: ChemicalSolution -> [RCHAM ()]
  deliver ChemSol{sentMessages, definitionSites} =
    definitionSites & concatMapWithKeyToList \siteId def@MkDefinitionSite{siteDeliveredMessages} ->
      let deliverable = Maybe.fromMaybe mempty $ Map.lookup siteId sentMessages
       in deliverable & multiSetMapMaybeToList \msg -> Just do
            removeSentMessage msg
            addDefinitionSite siteId def{siteDeliveredMessages = MultiSet.insert msg siteDeliveredMessages}
  react :: ChemicalSolution -> [RCHAM ()]
  react ChemSol{definitionSites, sentMessages} =
    definitionSites & concatMapWithKeyToList \definitionId def@MkDefinitionSite{siteDeliveredMessages, siteDefinition} ->
      case siteDefinition of
        Left (MkNativeDefinition nativeName pattern) ->
          satisfying siteDeliveredMessages pattern <&> \(match, consumed, _) -> do
            case Map.lookup nativeName nativeDefinitions of
              Nothing -> halt $ "Unknown native process: " <> nativeName
              Just (_, action) -> do
                addDefinitionSite definitionId def{siteDeliveredMessages = MultiSet.difference siteDeliveredMessages consumed}
                action match
        Right (MkIntepretedDefinition env definition) ->
          toReactionRules definition & concatMap \(pattern, body) ->
            satisfying siteDeliveredMessages pattern <&> \(match, consumed, _) -> do
              addDefinitionSite definitionId def{siteDeliveredMessages = MultiSet.difference siteDeliveredMessages consumed}
              addProcessThread (MkProcessThread (MkEnv $ Map.union match (getEnv env)) body)
  toReactionRules :: Syntax.Initial.Definition String -> [(Syntax.Initial.Pattern String, Syntax.Initial.Process String)]
  toReactionRules = \case
    Syntax.Initial.DefVoid -> []
    (Syntax.Initial.DefComposition def1 def2) -> toReactionRules def1 <> toReactionRules def2
    (Syntax.Initial.DefReactionRule pattern process) -> [(pattern, process)]
  satisfying consumable = \case
    Syntax.Initial.PatMessage (Syntax.Initial.MkName patChannelName) patMsg ->
      consumable & multiSetMapMaybeToList \msg@(MkSentMessage (MkChannelReference _ msgChannelName) sentMsg) ->
        if patChannelName == msgChannelName && length patMsg == length sentMsg
          then
            Just
              ( Map.fromList (zip (Coerce.coerce @_ @[String] patMsg) sentMsg)
              , MultiSet.singleton msg
              , MultiSet.delete msg consumable
              )
          else Nothing
    Syntax.Initial.PatSynchronization pat1 pat2 -> do
      (match1, consumed1, consumable) <- satisfying consumable pat1
      (match2, consumed2, consumable) <- satisfying consumable pat2
      pure (match1 <> match2, consumed1 <> consumed2, consumable)

nativeDefinitions ::
  Map
    String
    ( Syntax.Initial.Pattern String
    , Map String RTValue -> RCHAM ()
    )
nativeDefinitions =
  Map.fromList
    [
      ( "output"
      ,
        ( "output" S.|>> ["arg"]
        , \args -> case Map.lookup "arg" args of
            Nothing -> halt "Missing argument for output"
            Just val -> output val
        )
      )
    ]

pickStep ::
  (NonEmpty (m result) -> t m result) ->
  [input -> [m result]] ->
  input ->
  Maybe (t m result)
pickStep pick possibleActions input =
  case concatMap ($ input) possibleActions of
    [] -> Nothing
    result : more -> Just $ pick (result :| more)

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

mapMaybeWithKeyToList :: (k -> a -> Maybe b) -> Map k a -> [b]
mapMaybeWithKeyToList f =
  Maybe.mapMaybe (uncurry f) . Map.toList

concatMapWithKeyToList :: (k -> a -> [b]) -> Map k a -> [b]
concatMapWithKeyToList f =
  concatMap (uncurry f) . Map.toList

-- Core types

data ChemicalSolution = ChemSol
  { definitionSites :: !(Map DefinitionSiteId DefinitionSite)
  , processThreads :: !(MultiSet ProcessThread)
  , sentMessages :: !(Map DefinitionSiteId (MultiSet SentMessage))
  }
  deriving (Eq, Ord, Show)

newtype DefinitionSiteId = MkDefinitionSiteId Word64
  deriving (Eq, Ord, Show)

data ChannelReference = MkChannelReference !DefinitionSiteId !String
  deriving (Eq, Ord, Show)

data RTValue where
  RTChannelReference :: !ChannelReference -> RTValue
  RTPrimitive :: !RTPrimitive -> RTValue
  deriving (Eq, Ord, Show)

data RTPrimitive where
  PrimInteger :: !Integer -> RTPrimitive
  PrimDouble :: !Double -> RTPrimitive
  deriving (Eq, Ord, Show)

newtype Env = MkEnv {getEnv :: Map String RTValue}
  deriving (Eq, Ord, Show)

data ProcessThread = MkProcessThread
  { processEnv :: Env
  , processBody :: Syntax.Initial.Process String
  }
  deriving (Eq, Ord, Show)

data DefinitionSite = MkDefinitionSite
  { siteDeliveredMessages :: !(MultiSet SentMessage)
  , siteDefinition :: !(Either NativeDefinition InterpretedDefinition)
  }
  deriving (Eq, Ord, Show)

data InterpretedDefinition = MkIntepretedDefinition
  { definitionEnv :: !Env
  , interpretedDefinition :: !(Syntax.Initial.Definition String)
  }
  deriving (Eq, Ord, Show)

data NativeDefinition = MkNativeDefinition
  { nativeName :: !String
  , nativePattern :: !(Syntax.Initial.Pattern String)
  }
  deriving (Eq, Ord, Show)

data SentMessage = MkSentMessage
  { sentToChannel :: !ChannelReference
  , sentPayload :: ![RTValue]
  }
  deriving (Eq, Ord, Show)

data ExecutionError = MkExecutionError !ChemicalSolution !String
  deriving (Eq, Ord, Show)

newtype Fresh = MkFresh {getFresh :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

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

halt :: String -> RCHAM a
halt msg = do
  chem <- getChem
  haltE (MkExecutionError chem msg)

haltE :: ExecutionError -> RCHAM a
haltE = MkRCHAM . Except.throwE

-- ChemicalState

type ChemicalStateT = StateT ChemicalSolution

emptyChem :: ChemicalSolution
emptyChem = ChemSol mempty mempty mempty

addProcessThread :: ProcessThread -> RCHAM ()
addProcessThread p = modifyChem \chem ->
  chem{processThreads = MultiSet.insert p (processThreads chem)}

removeProcessThread :: ProcessThread -> RCHAM ()
removeProcessThread p = modifyChem \chem ->
  chem{processThreads = MultiSet.delete p (processThreads chem)}

addDefinitionSite :: DefinitionSiteId -> DefinitionSite -> RCHAM ()
addDefinitionSite id thread = modifyChem \chem ->
  chem{definitionSites = Map.insert id thread (definitionSites chem)}

removeDefinitionThread :: DefinitionSiteId -> RCHAM ()
removeDefinitionThread id = modifyChem \chem ->
  chem{definitionSites = Map.delete id (definitionSites chem)}

addSentMessage :: SentMessage -> RCHAM ()
addSentMessage msg@MkSentMessage{sentToChannel = MkChannelReference site _} = modifyChem \chem ->
  chem{sentMessages = Map.insertWith (<>) site (MultiSet.singleton msg) (sentMessages chem)}

removeSentMessage :: SentMessage -> RCHAM ()
removeSentMessage msg@MkSentMessage{sentToChannel = MkChannelReference site _} = modifyChem \chem ->
  chem{sentMessages = Map.update removeMsg site (sentMessages chem)}
 where
  removeMsg (MultiSet.delete msg -> msgs')
    | MultiSet.null msgs' = Nothing
    | otherwise = Just msgs'

putChem :: ChemicalSolution -> RCHAM ()
putChem = MkRCHAM . Trans.lift . State.put

getChem :: RCHAM ChemicalSolution
getChem = MkRCHAM . Trans.lift $ State.get

modifyChem :: (ChemicalSolution -> ChemicalSolution) -> RCHAM ()
modifyChem = MkRCHAM . Trans.lift . State.modify'

-- Output

type OutputT = WriterT (DList RTValue)

output :: RTValue -> RCHAM ()
output = MkRCHAM . Trans.lift . Trans.lift . Writer.tell . DList.singleton

-- Fresh name

type FreshNameT = StateT Fresh

fresh :: RCHAM Word64
fresh = MkRCHAM do
  MkFresh w <- Trans.lift . Trans.lift . Trans.lift $ State.get
  Trans.lift . Trans.lift . Trans.lift . State.put $ MkFresh (w + 1)
  pure w

freshDefinitionSiteId :: RCHAM DefinitionSiteId
freshDefinitionSiteId = MkDefinitionSiteId <$> fresh
