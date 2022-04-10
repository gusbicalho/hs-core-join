{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
                  |> [ "val" |<< ["v"]
                     , "put_ret" |<< []
                     ]
              , [ "get" |>> ["get_ret"]
                , "val" |>> ["v"]
                ]
                  |> [ "get_ret" |<< []
                     ]
              ]
              [ "val" |<< ["initialValue"]
              , "k" |<< ["put", "get"]
              ]
           ]
    ]
    [ "print" |<< [litI 1, litD 2.3]
    ]

ex1Initial :: Syntax.Initial.Process
ex1Initial = ex1 :: Syntax.Initial.Process

-- >>> Syntax.Initial.freeVariables ex1Initial
-- fromList [Name "print"]
