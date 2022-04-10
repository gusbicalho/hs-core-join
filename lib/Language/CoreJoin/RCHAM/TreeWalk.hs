{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.MultiSet (MultiSet)
import Data.Set (Set)
import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial

eval :: Syntax.Initial.Process -> ([OutputItem], Maybe ExecutionError)
eval =
  unwrap
    . Identity.runIdentity
    . Writer.runWriterT
    . flip State.evalStateT initialState
    . Except.runExceptT
    . runRCHAM
    . rcham
 where
  unwrap (result, output) =
    (DList.toList output, either Just (const Nothing) result)

rcham :: Syntax.Initial.Process -> RCHAM ()
rcham _ = pure ()

-- Runtime state

data ChemicalSolution = ChemSol
  { processThreads :: !(MultiSet Syntax.Initial.Process)
  , definitionThreads :: !(MultiSet Syntax.Initial.Definition)
  }

data OutputItem
  = OutputD Double
  | OutputI Integer

newtype ExecutionError = MkExecutionError String

-- Execution monad

newtype RCHAM a = MkRCHAM
  { runRCHAM ::
      ExceptT
        ExecutionError
        ( StateT
            ChemicalSolution
            ( WriterT
                (DList OutputItem)
                Identity
            )
        )
        a
  }
  deriving newtype (Functor, Applicative, Monad)

initialState :: ChemicalSolution
initialState = ChemSol mempty mempty

output :: OutputItem -> RCHAM ()
output = MkRCHAM . Trans.lift . Trans.lift . Writer.tell . DList.singleton

halt :: ExecutionError -> RCHAM ()
halt = MkRCHAM . Except.throwE

put :: ChemicalSolution -> RCHAM ()
put = MkRCHAM . Trans.lift . State.put

get :: RCHAM ChemicalSolution
get = MkRCHAM . Trans.lift $ State.get

update :: (ChemicalSolution -> ChemicalSolution) -> RCHAM ()
update = MkRCHAM . Trans.lift . State.modify'
