module Language.CoreJoin.RCHAM.TreeWalk where

import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial

data OutputItem
  = OutputD Double
  | OutputI Integer

newtype ExecutionError = MkExecutionError String

eval :: Syntax.Initial.Process -> ([OutputItem], Maybe ExecutionError)
eval _ = ([], Nothing)
