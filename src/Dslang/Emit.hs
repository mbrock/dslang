{-# Language RecursiveDo #-}

module Dslang.Emit where

import Dslang.Base
import Dslang.Tree
import Dslang.Grok

import EVM.Assembly hiding (emit)
import EVM.Keccak (abiKeccak)

emit :: [(Part, Env)] -> [Instr]
emit xs = assemble $ mdo
  mapM_ (emitPart fail) xs
  fail <- label
  revert

emitPart :: Label -> (Part, Env) -> Assembly
emitPart fail (part, env) = mdo
  push 224; push 2; exp; push 0; calldataload; div
  let theName = (view partName part)
  as (unpack theName) $
    push (fromIntegral (abiKeccak (encodeUtf8 theName)))
  eq; not; refer next; jumpi

  mapM_ (emitLine (part, env) fail) (view partCode part)
  stop
  next <- label
  pure ()

emitLine :: (Part, Env) -> Label -> Line -> Assembly
emitLine (part, env) fail = \case
  LetLine (Var t _) e -> do
    expr env e
    push (tempIndex env t)
    mstore
  SetLine (Var t _) e -> do
    expr env e
    push (slotIndex env t)
    sstore
  IffLine e -> do
    expr env e
    not
    refer fail
    jumpi
  LogLine es -> do
    pure ()

tempIndex :: Env -> Text -> Integer
tempIndex env t =
  (* 32) . fromIntegral . fromJust . elemIndex t . sort . toList $
    view envTemps env

slotIndex :: Env -> Text -> Integer
slotIndex env t =
  fromIntegral . fromJust . elemIndex t . sort . toList $
    view envSlots env

expr :: Env -> Expr -> Assembly
expr env =
  let
    bin x y f = do expr env x; expr env y; f
  in
    \case
      EAdd x y -> bin x y add
      ESub x y -> bin x y sub
      EMul x y -> bin x y mul
      EDiv x y -> bin x y div
      EExp x y -> bin x y exp
      EAnd x y -> bin x y and
      EOr  x y -> bin x y or
      ELt  x y -> bin x y lt
      EGt  x y -> bin x y gt
      ELte x y -> bin x y lt
      EGte x y -> bin x y gt
      EEq  x y -> bin x y eq
      EVar (Var v _) ->
        if elem v (view envSlots env)
        then do as (unpack v) (push (slotIndex env v)); sload
        else do as (unpack v) (push (tempIndex env v)); mload
      ENow -> timestamp
      ECaller -> caller
      EWord x -> push x
