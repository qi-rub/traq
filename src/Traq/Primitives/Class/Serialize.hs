{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Class.Serialize (
  SerializePrim (..),
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude

-- --------------------------------------------------------------------------------
-- Printing and Parsing
-- --------------------------------------------------------------------------------

{- | Simple class to print/parse second-order primitives.
Syntax: `@prim<args, ...>[fns, ...];`
-}
class SerializePrim prim where
  -- | all names (variants) used by the primitive
  primNames :: [Ident]

  -- | name for a specific constructor
  primNameOf :: prim -> Ident
  primNameOf _ = case (primNames @prim) of
    [s] -> s
    _ -> error "primitive has multiple names, please override primNameOf"

  -- | parse the primitive info given its name
  parsePrimParams :: (SizeType prim ~ Sym.Sym SizeT) => TokenParser () -> Ident -> Parser prim

  -- | print the primitives parameters.
  printPrimParams :: prim -> [String]
