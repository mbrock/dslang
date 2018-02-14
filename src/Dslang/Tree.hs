{-# Language TemplateHaskell #-}

module Dslang.Tree where

import Dslang.Base

data Primed = Primed | Unprimed
  deriving (Eq, Show, Data)

data Var = Var Text Primed | Prop Text Primed
  deriving (Eq, Show, Data)

data Expr
  = EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EExp Expr Expr
  | EAnd Expr Expr
  | EOr  Expr Expr
  | EEq  Expr Expr
  | ELt  Expr Expr
  | EGt  Expr Expr
  | ELte Expr Expr
  | EGte Expr Expr
  | EVar Var
  | ENow
  | ECaller
  | EWord Integer
  deriving (Eq, Show, Data)

data Line
  = LetLine Var Expr
  | SetLine Var Expr
  | IffLine Expr
  | LogLine [Expr]
  deriving (Eq, Show, Data)

data Part = Part
  { _partName :: Text
  , _partArgs :: [Var]
  , _partCode :: [Line]
  } deriving (Eq, Show, Data)
makeLenses ''Part

data Code = Code [Part]
  deriving (Eq, Show, Data)

makePrisms ''Var
makePrisms ''Expr
makePrisms ''Line
