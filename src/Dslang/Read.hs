{-# Language TemplateHaskell #-}

module Dslang.Read where

import Dslang.Base
import Dslang.Tree

import Text.Parser.Expression
import Text.Parser.LookAhead
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Trifecta hiding (parseFromFile)
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Text.Trifecta as Trifecta

parseFromFile :: MonadIO m => String -> m (Maybe Code)
parseFromFile = Trifecta.parseFromFile (runUnlined pCode)

pCode = (Code <$> sepBy1 pPart newline) <* eof

pPart =
  Part
    <$> name
    <*> parens pArgs <* char ':' <* newline <* newline
    <*> some pLine

pArgs = sepBy pVar (text ", ")

pLine = text "   " *> choice ps <* newline
  where
    ps =
      [ LetLine
          <$> (text "let " *> token pVar)
          <*> (text "= " *> pExpr)
      , SetLine
          <$> (text "set " *> token pVar)
          <*> (text "= " *> pExpr)
      , IffLine
          <$> (text "iff " *> pExpr)
      , LogLine
          <$> (text "log " *> sepBy1 pExpr (text ", "))
      ]

pExpr =
  buildExpressionParser table term
    <?> "expression"
  where
    table =
      [ [ binary "^" EExp AssocRight ]
      , [ binary "·" EMul AssocLeft, binary "/" EDiv AssocLeft ]
      , [ binary "+" EAdd AssocLeft, binary "−" ESub AssocLeft ]
      , [ binary "≤" ELt AssocLeft, binary "≥" EGt AssocLeft, binary "=" EEq AssocLeft ]
      , [ binary "∧" EAnd AssocLeft, binary "∨" EOr AssocLeft ]
      ]
    term =
      parens pExpr
        <|> (EVar <$> token pVar <?> "variable name")
        <|> (token pConst <?> "constant")
        <|> (EWord <$> token integer' <?> "word")

binary name f assoc = Infix (f <$ (reserveText idents name) <??> name) assoc

pVar = do
  x <- name
  if "′" `isSuffixOf` x
    then pure (Var (Text.take (Text.length x - 1) x) Primed)
    else pure (Var x Unprimed)

pConst = choice [text "@" *> pure ECaller]

name = pack <$> some (noneOf " \n·/,()[]0123456789")

idents = emptyOps { _styleReserved = reserveds }

reserveds :: HashSet.HashSet String
reserveds = HashSet.fromList ["let", "set", "iff", "log", "now"]

a <??> b = a <?> unpack ("‘" <> b <> "’")
