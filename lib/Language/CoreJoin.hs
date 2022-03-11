{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.CoreJoin (someFunc) where

import Data.Kind (Type)
import Data.String (IsString (fromString))

{-

From the paper:

P, Q, R ::=       processes
     x<y~z>       send asynchronous message
  || def D in P   local definition
  || P | Q        parallel composition
  || 0            inert process

D ::=             definitions
     J |> P       reaction rule
  || D /\ D'      composition
  || T            void definition

J ::=             join patterns
     x<y~z>       message pattern
  || J | J'       synchronization

-}

ex1 =
  def
    [ "newVar" |>> ["initialValue", "k"]
        |> def
          [ "put" |>> ["w", "put_ret"]
              <> "val" |>> ["v"]
                |> ("val" |<< ["v"] <> "put_ret" |<< [])
          , "get" |>> ["get_ret"]
              <> "val" |>> ["v"]
                |> ("get_ret" |<< [])
          ]
          ("val" |<< ["initialValue"] <> "k" |<< ["put", "get"])
    ]
    $ "print" |<< [litI 1]

-- >>> :i <>
-- type Semigroup :: * -> Constraint
-- class Semigroup a where
--   (<>) :: a -> a -> a
--   ...
--   	-- Defined in ‘GHC.Base’
-- infixr 6 <>

-- Sugar

cat :: Monoid a => [a] -> a
cat = mconcat

(|>>) :: Name -> [Name] -> PatternSyntax
(|>>) = PatMessage
infixr 7 |>>

def :: [DefinitionSyntax] -> ProcessSyntax -> ProcessSyntax
def defs = ProcLocalDef (mconcat defs)

(|>) = DefReactionRule
infixr 5 |>

(|<<) :: Name -> [ValueSyntax] -> ProcessSyntax
(|<<) = ProcSend
infixr 7 |<<

litI :: Integer -> ValueSyntax
litI = ValueLiteral . LitInteger

-- Types

data ProcessSyntax where
  ProcSend :: Name -> [ValueSyntax] -> ProcessSyntax
  ProcLocalDef :: DefinitionSyntax -> ProcessSyntax -> ProcessSyntax
  ProcParallel :: ProcessSyntax -> ProcessSyntax -> ProcessSyntax
  ProcInert :: ProcessSyntax
  deriving stock (Eq, Ord, Show)

instance Semigroup ProcessSyntax where
  (<>) = ProcParallel

instance Monoid ProcessSyntax where
  mempty = ProcInert

data DefinitionSyntax where
  DefReactionRule :: PatternSyntax -> ProcessSyntax -> DefinitionSyntax
  DefComposition :: DefinitionSyntax -> DefinitionSyntax -> DefinitionSyntax
  DefVoid :: DefinitionSyntax
  deriving stock (Eq, Ord, Show)

instance Semigroup DefinitionSyntax where
  (<>) = DefComposition

instance Monoid DefinitionSyntax where
  mempty = DefVoid

data PatternSyntax where
  PatMessage :: Name -> [Name] -> PatternSyntax
  PatSynchronization :: PatternSyntax -> PatternSyntax -> PatternSyntax
  deriving stock (Eq, Ord, Show)

instance Semigroup PatternSyntax where
  (<>) = PatSynchronization

data ValueSyntax where
  ValueLiteral :: LiteralSyntax -> ValueSyntax
  ValueVarLookup :: Name -> ValueSyntax
  deriving stock (Eq, Ord, Show)

instance IsString ValueSyntax where
  fromString = ValueVarLookup . fromString

data LiteralSyntax where
  LitInteger :: Integer -> LiteralSyntax
  deriving stock (Eq, Ord, Show)

newtype Name = Name String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
