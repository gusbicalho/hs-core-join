{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.CoreJoin where

import Data.Foldable (foldl', toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import GHC.Stack (HasCallStack)

import Language.CoreJoin.RCHAM.TreeRewrite qualified as TreeRewrite
import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial
import Language.CoreJoin.Syntax.Sugar (
  def,
  litD,
  litI,
  (|<<),
  (|>),
  (|>>),
 )
import Language.CoreJoin.Syntax.Sugar qualified as S

-- Example

ex1 ::
  ( S.Sugar syntax
  , S.Literal syntax Integer
  , S.Literal syntax Double
  ) =>
  S.ProcessSyntax syntax
ex1 =
  def
    [ ["newVar" |>> ["initialValue", "k"]]
        |> [ def
              [ [ "put" |>> ["w", "put_ret"]
                , "val" |>> ["v"]
                ]
                  |> [ "val" |<< ["w"]
                     , "put_ret" |<< []
                     ]
              , [ "get" |>> ["get_ret"]
                , "val" |>> ["v"]
                ]
                  |> [ "val" |<< ["v"]
                     , "get_ret" |<< ["v"]
                     ]
              ]
              [ "val" |<< ["initialValue"]
              , "k" |<< ["put", "get"]
              ]
           ]
    ]
    [ "output" |<< [litI 1]
    , "output" |<< [litD 2.3]
    , def
        [ ["varCreated" |>> ["put", "get"]]
            |> [ "get" |<< ["output"]
               , def
                  [ ["didPut" |>> []]
                      |> ["get" |<< ["output"]]
                  ]
                  ["put" |<< [litI 20, "didPut"]]
               ]
        ]
        ["newVar" |<< [litI 10, "varCreated"]]
    ]

ex1Initial :: Syntax.Initial.Process String
ex1Initial = ex1 :: Syntax.Initial.Process String

-- >>> Syntax.Initial.freeVariables ex1Initial
-- fromList ["output"]

-- >>> fst $ TreeRewrite.eval ex1
-- ([OutputI 1,OutputD 2.3,OutputI 10,OutputI 20],Just (MkExecutionError (ChemSol {processThreads = fromOccurList [(ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 4, nameString = "val"}})) [ValueLiteral (LitInteger 20)],1)], definitionThreads = fromOccurList [(NativeDefinition "output" (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "output"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "arg"}}]),1),(DefinitionAST (DefReactionRule (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "newVar"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "initialValue"}},MkName {getName = MkLocalName {nameId = 0, nameString = "k"}}]) (ProcLocalDef (DefComposition (DefReactionRule (PatSynchronization (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "val"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "v"}}]) (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "put"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "w"}},MkName {getName = MkLocalName {nameId = 0, nameString = "put_ret"}}])) (ProcParallel (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "val"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "w"}})]) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "put_ret"}})) []))) (DefReactionRule (PatSynchronization (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "val"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "v"}}]) (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "get"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "get_ret"}}])) (ProcParallel (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "val"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "v"}})]) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "get_ret"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "v"}})])))) (ProcParallel (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "val"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "initialValue"}})]) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "k"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "put"}}),ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "get"}})])))),1),(DefinitionAST (DefReactionRule (PatMessage (MkName {getName = MkLocalName {nameId = 1, nameString = "varCreated"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "put"}},MkName {getName = MkLocalName {nameId = 0, nameString = "get"}}]) (ProcParallel (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "get"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "output"}})]) (ProcLocalDef (DefReactionRule (PatMessage (MkName {getName = MkLocalName {nameId = 0, nameString = "didPut"}}) []) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "get"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "output"}})])) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "put"}})) [ValueLiteral (LitInteger 20),ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "didPut"}})])))),1),(DefinitionAST (DefReactionRule (PatMessage (MkName {getName = MkLocalName {nameId = 5, nameString = "didPut"}}) []) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 2, nameString = "get"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "output"}})])),1),(DefinitionAST (DefReactionRule (PatSynchronization (PatMessage (MkName {getName = MkLocalName {nameId = 4, nameString = "val"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "v"}}]) (PatMessage (MkName {getName = MkLocalName {nameId = 2, nameString = "get"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "get_ret"}}])) (ProcParallel (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 4, nameString = "val"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "v"}})]) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "get_ret"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "v"}})]))),1),(DefinitionAST (DefReactionRule (PatSynchronization (PatMessage (MkName {getName = MkLocalName {nameId = 4, nameString = "val"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "v"}}]) (PatMessage (MkName {getName = MkLocalName {nameId = 3, nameString = "put"}}) [MkName {getName = MkLocalName {nameId = 0, nameString = "w"}},MkName {getName = MkLocalName {nameId = 0, nameString = "put_ret"}}])) (ProcParallel (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 4, nameString = "val"}})) [ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "w"}})]) (ProcSend (ValueVarLookup (MkName {getName = MkLocalName {nameId = 0, nameString = "put_ret"}})) []))),1)]}) "Deadlock!"))
