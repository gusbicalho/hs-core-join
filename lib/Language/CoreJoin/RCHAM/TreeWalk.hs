{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.CoreJoin.RCHAM.TreeWalk where

import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.Writer.CPS (Writer, WriterT)
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Functor.Identity (Identity)
import Data.Functor.Identity qualified as Identity
import Data.Kind (Type)
import Data.Maybe qualified as Maybe
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Word (Word64)
import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial
import Utils.Transformers qualified as Utils.Trans

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
  initialProcess <- traverse freshName rawProcess
  addProcessThread initialProcess
  whileM ((emptyChem ==) <$> getChem) do
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
  str_null chem = []
  str_par chem = []
  str_top chem = []
  str_and chem = []
  str_def chem = []
  react chem = []

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
  { processThreads :: !(MultiSet (Syntax.Initial.Process Name))
  , definitionThreads :: !(MultiSet (Syntax.Initial.Definition Name))
  }
  deriving (Eq, Show)

data OutputItem
  = OutputD Double
  | OutputI Integer

data ExecutionError = MkExecutionError ChemicalSolution String

newtype Fresh = MkFresh Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data Name = MkName {nameId :: Word64, nameString :: String}
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

addProcessThread :: Syntax.Initial.Process Name -> RCHAM ()
addProcessThread p = modifyChem \chem ->
  chem{processThreads = MultiSet.insert p (processThreads chem)}

addDefinitionThread :: Syntax.Initial.Definition Name -> RCHAM ()
addDefinitionThread p = modifyChem \chem ->
  chem{definitionThreads = MultiSet.insert p (definitionThreads chem)}

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

freshName :: String -> RCHAM Name
freshName s = MkRCHAM do
  MkFresh id <- Trans.lift . Trans.lift . Trans.lift $ State.get
  Trans.lift . Trans.lift . Trans.lift . State.put $ MkFresh (id + 1)
  pure MkName{nameId = id, nameString = s}
