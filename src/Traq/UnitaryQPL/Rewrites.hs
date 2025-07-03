module Traq.UnitaryQPL.Rewrites where

import Traq.UnitaryQPL.Syntax

elimHolesS :: Stmt holeT sizeT -> Stmt' sizeT
elimHolesS HoleS{} = error "cannot eliminate hole"
elimHolesS _ = error "TODO"
