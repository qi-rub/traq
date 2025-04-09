module QCompose.CQPL.Lowering (
  lowerProgram,
  QSearchCQImpl,
) where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.CQPL.Syntax
import QCompose.Control.MonadHelpers
import qualified QCompose.Data.Context as Ctx
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

-- | Formulas for primitives
newtype QSearchCQImpl sizeT costT = QSearchCQImpl
  { costFormulas :: P.QSearchFormulas sizeT costT
  }

-- | Configuration for lowering
type LoweringConfig sizeT costT = (P.FunCtx sizeT, QSearchCQImpl sizeT costT)

protoFunCtx :: Lens' (LoweringConfig sizeT costT) (P.FunCtx sizeT)
protoFunCtx = _1

qsearchConfig :: Lens' (LoweringConfig sizeT costT) (QSearchCQImpl sizeT costT)
qsearchConfig = _2

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
type LoweringCtx sizeT = (Set.Set Ident, P.TypingCtx sizeT)

emptyLoweringCtx :: LoweringCtx sizeT
emptyLoweringCtx = (Set.empty, Ctx.empty)

uniqNames :: Lens' (LoweringCtx sizeT) (Set.Set Ident)
uniqNames = _1

typingCtx :: Lens' (LoweringCtx sizeT) (P.TypingCtx sizeT)
typingCtx = _2

-- | The outputs of lowering
type LoweringOutput sizeT costT = ([ProcDef sizeT], [UQPL.ProcDef sizeT costT])

loweredProcs :: Lens' (LoweringOutput sizeT costT) [ProcDef sizeT]
loweredProcs = _1

loweredUProcs :: Lens' (LoweringOutput sizeT costT) [UQPL.ProcDef sizeT costT]
loweredUProcs = _2

{- | Monad to compile ProtoQB to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT sizeT costT = RWST (LoweringConfig sizeT costT) (LoweringOutput sizeT costT) (LoweringCtx sizeT) (Either String)

newIdent :: forall sizeT costT. Ident -> CompilerT sizeT costT Ident
newIdent prefix = do
  ident <-
    msum . map checked $
      prefix : map ((prefix <>) . ("_" <>) . show) [1 :: Int ..]
  uniqNames . at ident ?= ()
  return ident
 where
  checked :: Ident -> CompilerT sizeT cosT Ident
  checked name = do
    already_exists <- use (uniqNames . at name)
    case already_exists of
      Nothing -> return name
      Just () -> throwError "next ident please!"

-- | Allocate an ancilla register, and update the typing context.
allocAncilla :: P.VarType sizeT -> CompilerT sizeT costT Ident
allocAncilla ty = do
  name <- newIdent "aux"
  zoom typingCtx $ Ctx.put name ty
  return name

-- | Add a new procedure.
addProc :: ProcDef sizeT -> CompilerT sizeT costT ()
addProc procDef = tell $ mempty & loweredProcs .~ [procDef]

-- | lower an oracle declaration to a uqpl oracle decl
lowerOracleDecl :: P.OracleDecl sizeT -> OracleDecl sizeT
lowerOracleDecl P.OracleDecl{P.param_types, P.ret_types} =
  OracleDecl{oracle_param_types = param_types ++ ret_types}

-- | Lower a full program into a UQPL program.
lowerProgram ::
  forall sizeT costT.
  (Floating costT) =>
  -- | the implementation of primitive `any`
  QSearchCQImpl sizeT costT ->
  -- | input bindings to the source program
  P.TypingCtx sizeT ->
  -- | fail prob \( \varepsilon \)
  costT ->
  -- | source program
  P.Program sizeT ->
  Either String (Program sizeT, P.TypingCtx sizeT)
lowerProgram qsearch_config gamma_in delta prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config = (funCtx, qsearch_config)
  let lowering_ctx =
        emptyLoweringCtx
          & typingCtx .~ gamma_in
          & uniqNames .~ P.allNamesP prog

  let compiler = error "TODO -- lowerStmt delta stmt"
  (stmtQ, lowering_ctx', outputU) <- runRWST compiler config lowering_ctx

  return
    ( Program
        { oracle_decl = funCtx & P.oracle_decl & lowerOracleDecl
        , proc_defs = outputU ^. loweredProcs
        , uproc_defs = outputU ^. loweredUProcs
        , stmt = stmtQ
        }
    , lowering_ctx' ^. typingCtx
    )
