module QCompose.UnitaryQPL.Rewrites where

import Data.Void (Void)
import QCompose.UnitaryQPL.Syntax

elimHolesS :: Stmt holeT sizeT costT -> Stmt Void sizeT costT
elimHolesS HoleS{} = error "cannot eliminate hole"
elimHolesS _ = error "TODO"
