module Traq.Primitives.Prelude (parsePrimWithPredicates) where

import Control.Monad (replicateM)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (..), TokenParser)

import Traq.Prelude

{- | Parse a primitive with a list of predicate names, each bound to a set of arguments.
 Example: @parsePrimWithPredicates "any" 1@ can be used to implement the parser for @any@.
-}
parsePrimWithPredicates :: Ident -> Int -> TokenParser () -> Parser [(Ident, [Ident])]
parsePrimWithPredicates prim_name n_preds tp = do
  _ <- symbol tp $ "@" ++ prim_name
  replicateM n_preds $ do
    predicate <- brackets tp $ identifier tp
    args <- parens tp $ commaSep tp $ identifier tp
    pure (predicate, args)
