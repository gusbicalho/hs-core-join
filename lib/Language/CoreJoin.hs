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
import Language.CoreJoin.RCHAM.TreeRewriteWithMsgPool qualified as TreeRewriteWithMsgPool
import Language.CoreJoin.RCHAM.TreeRewriteWithRandom qualified as TreeRewriteWithRandom
import Language.CoreJoin.Syntax.Initial qualified as Syntax.Initial
import Language.CoreJoin.Syntax.Sugar (
  def,
  litD,
  litI,
  (?*),
  (?-),
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
  {-
    # Local definitions:

    The core join calculus expr
        def a /\ b /\ c
         in p1 | p2 | p3
    can be directly translated to this form:
        def [a,b,c] [p1,p2,p3]
    or we can use the inverted forms:
        [p1,p2,p3] `where_` [a,b,c]
        [p1,p2,p3] ?* [a,b,c]
        p1 ?- a (single proc, single definition)

    The `?-` form is especially useful when defining sequences of
    request/response steps. These are defined using continuation processes,
    which usually end up looking like this:

        "fstStep" |<< ["argument", "did_fstStep"]
          ?- ["did_fstStep" |>> ["fstResult"]]
            |> [ "sndStep" |<< ["argument2", "did_sndStep"]
                  ?- ["did_sndStep" |>> ["sndResult"]]
                    |> [do more steps]
              ]

    This is not horrible, but this is what it would look like with `def`:

        def
          [ ["did_fstStep" |>> ["fstResult"]]
              |> [ def
                    [ ["did_sndStep" |>> ["sndResult"]]
                        |> [do more steps]
                    ]
                    ["sndStep" |<< ["argument2", "did_sndStep"]]
                ]
          ]
          ["fstStep" |<< ["argument", "did_fstStep"]]
  -}
  def
    [ ["newMVar" |>> ["newMVar_ret"]]
        |> [ ["newMVar_ret" |<< ["put", "take"]]
              ?* [ ["put" |>> ["newVal", "put_ret"]]
                    |> [ "val" |<< ["newVal"]
                       , "put_ret" |<< []
                       ]
                 , [ "take" |>> ["take_ret"]
                   , "val" |>> ["oldVal"]
                   ]
                    |> ["take_ret" |<< ["oldVal"]]
                 ]
           ]
    , ["newFilledMVar" |>> ["initialValue", "newFilledMVar_ret"]]
        |> [ "newMVar" |<< ["did_newMVar"]
              ?- ["did_newMVar" |>> ["put", "take"]]
                |> [ "put" |<< ["initialValue", "did_put"]
                      ?- ["did_put" |>> []]
                        |> ["newFilledMVar_ret" |<< ["put", "take"]]
                   ]
           ]
    , ["newAtom" |>> ["initialValue", "newAtom_ret"]]
        |> [ "newFilledMVar" |<< ["initialValue", "did_newFilledMVar"]
              ?- ["did_newFilledMVar" |>> ["put", "take"]]
                |> [ "newAtom_ret" |<< ["swap"]
                      ?- ["swap" |>> ["f", "swap_ret"]]
                        |> [ "take" |<< ["did_take"]
                              ?- ["did_take" |>> ["oldVal"]]
                                |> [ "f" |<< ["oldVal", "didRun"]
                                      ?- ["didRun" |>> ["newVal"]]
                                        |> [ "put" |<< ["newVal", "did_put"]
                                              ?- ["did_put" |>> []]
                                                |> ["swap_ret" |<< ["oldVal", "newVal"]]
                                           ]
                                   ]
                           ]
                   ]
           ]
    , ["identity" |>> ["v", "identity_ret"]]
        |> ["identity_ret" |<< ["v"]]
    , ["const" |>> ["v", "const_ret"]]
        |> [ "const_ret" |<< ["constFn"]
              ?- ["constFn" |>> ["_", "constFn_ret"]]
                |> ["constFn_ret" |<< ["v"]]
           ]
    , ["read" |>> ["atomSwap", "read_ret"]]
        |> [ "atomSwap" |<< ["identity", "did_swap"]
              ?- ["did_swap" |>> ["_", "val"]]
                |> [ "read_ret" |<< ["val"]
                   ]
           ]
    , ["reset" |>> ["atomSwap", "v", "reset_ret"]]
        |> [ "const" |<< ["v", "did_const"]
              ?- ["did_const" |>> ["constFn"]]
                |> [ "atomSwap" |<< ["constFn", "did_swap"]
                      ?- ["did_swap" |>> ["_", "val"]]
                        |> [ "reset_ret" |<< ["val"]
                           ]
                   ]
           ]
    ]
    [ "output" |<< [litI 1]
    , "output" |<< [litD 2.3]
    , "newAtom" |<< [litI 10, "did_newAtom"]
        ?- ["did_newAtom" |>> ["atom"]]
          |> [ "read" |<< ["atom", "output"]
             , "reset" |<< ["atom", litI 20, "did_reset"]
                ?- ["did_reset" |>> ["_"]]
                  |> [ "read" |<< ["atom", "output"]
                     ]
             ]
    ]

ex1Initial :: Syntax.Initial.Process String
ex1Initial = ex1 :: Syntax.Initial.Process String

-- >>> Syntax.Initial.freeVariables ex1Initial
-- fromList ["output"]

{-
TreeRewrite and TreeRewriteWithMsgPool are deterministic

>>> fst $ TreeRewrite.eval ex1
[OutputI 1,OutputD 2.3,OutputI 10,OutputI 20]

>>> fst $ TreeRewriteWithMsgPool.eval ex1
[OutputI 1,OutputD 2.3,OutputI 10,OutputI 20]


See how the output can be different if we add randomness!
There's a race between the first atom read and the write-and-read

>>> fst $ TreeRewriteWithRandom.eval 0 ex1
[OutputD 2.3,OutputI 1,OutputI 20,OutputI 20]

>>> fst $ TreeRewriteWithRandom.eval 3 ex1
[OutputI 1,OutputD 2.3,OutputI 10,OutputI 20]
-}
