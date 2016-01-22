{-# LANGUAGE NoMonomorphismRestriction #-}
module RE.Parse where

import Control.Applicative
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Parser
import Text.Trifecta.Result

import RE.AST

parse s = let Success ast = parseString expr (Lines 0 0 0 0) s in
    ast

expr = term `chainl1` or_op
    where or_op = char '|' *> (pure $ Binary Alternative)

term = factor `chainl1` and_op
    where and_op = pure $ Binary Sequence

factor = try (Unary <$> basic_char <*> unary_op)
    <|> basic_char

basic_char = Literal <$> noneOf "|*+?"

unary_op = (pure Many <* char '*')
    <|> (pure Many1 <* char '+')
    <|> (pure Optional <* char '?')
