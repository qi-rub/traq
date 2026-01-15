{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Serialize (
  SerializePrim (..),
) where

import Control.Applicative (Alternative ((<|>)), many)
import Control.Monad.Extra (concatMapM)
import Text.Parsec (try)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (..), TokenParser)
import Text.Printf (printf)

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Class.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

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

-- Pretty Printing
instance (SerializePrim prim) => PP.ToCodeString (Primitive prim) where
  build (Primitive par_funs prim) = do
    fns <- concatMapM PP.fromBuild par_funs
    PP.putWord $ printf "@%s<%s>[%s]" (primNameOf prim) (PP.commaList $ printPrimParams prim) fns

-- Parsing
instance (SerializePrim prim, SizeType prim ~ Sym.Sym SizeT) => P.Parseable (Primitive prim) where
  parseE tp@TokenParser{..} = do
    ('@' : name) <- foldr1 (<|>) $ map (\s -> try $ symbol $ "@" ++ s) $ primNames @prim
    prim <- angles $ parsePrimParams tp name
    par_funs <- brackets $ many $ P.parseE tp
    return $ Primitive par_funs prim
