{-# Language TupleSections #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Dslang.Grok where

import Dslang.Base
import Dslang.Tree

import Data.Generics.Uniplate.Data

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified EVM.Assembly as EVM

data Failure
  = XCircularDefinition
  | XSetUnprimed Text
  | XDoubleSlotAssignment Text
  | XDoubleLet Text
  | XPrimedLet Text
  | XUnprimedUse Text
  deriving (Show, Data)

data Env = Env
  { _envSlots :: Set Text
  , _envPrimed :: Set Text
  , _envTemps :: Set Text
  }
makeLenses ''Env

grok :: Code -> Either Failure [(Part, Env)]
grok (Code parts) = checkParts mempty parts

checkParts :: Set Text -> [Part] -> Either Failure [(Part, Env)]
checkParts _ [] =
  error "internal error"
checkParts oldSlots [x] = do
  y <- check oldSlots (view partCode x)
  pure [(x, y)]
checkParts oldSlots (x : xs) = do
  s <- check oldSlots (view partCode x)
  ((x, s) :) <$> checkParts (view envSlots s) xs

check :: Set Text -> [Line] -> Either Failure Env
check oldSlots lines =
  runExcept (execStateT (mapM_ go lines) init)

  where
    init = Env oldSlots mempty mempty

    checkExpr e = do
      slots <- use envSlots
      primed <- use envPrimed
      temps <- use envTemps
      case e of
        EVar (Var t Unprimed) ->
          if Set.member t primed
          then throw (XUnprimedUse t)
          else
            if Set.member t slots
            then pure ()
            else
              if Set.member t temps
              then pure ()
              else envSlots %= (Set.insert t)
        _ ->
          pure ()

    go (SetLine (Var t Unprimed) _) =
      throw (XSetUnprimed t)

    go (SetLine (Var t Primed) e) = do
      x <- use envPrimed
      when (Set.member t x) (throw (XDoubleSlotAssignment t))
      envSlots %= (Set.insert t)
      mapM_ checkExpr (universe e)
      envPrimed %= (Set.insert t)

    go (LetLine (Var t Unprimed) e) = do
      slots <- use envSlots
      temps <- use envTemps
      when (Set.member t slots) (throw (XDoubleLet t))
      when (Set.member t temps) (throw (XDoubleLet t))
      envTemps %= (Set.insert t)
      mapM_ checkExpr (universe e)

    go (LetLine (Var t Primed) e) =
      throw (XPrimedLet t)

    go (IffLine e) = do
      mapM_ checkExpr (universe e)

    go (LogLine es) = do
      mapM_ checkExpr (es >>= universe)
