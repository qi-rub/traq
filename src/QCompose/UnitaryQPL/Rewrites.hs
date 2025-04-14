module QCompose.UnitaryQPL.Rewrites where

import QCompose.UnitaryQPL.Syntax

elimHolesS :: Stmt holeT sizeT -> Stmt' sizeT
elimHolesS HoleS{} = error "cannot eliminate hole"
elimHolesS _ = error "TODO"
