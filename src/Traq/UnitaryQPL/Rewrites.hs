module Traq.UnitaryQPL.Rewrites where

import Traq.UnitaryQPL.Syntax

elimHolesS :: UStmt holeT sizeT -> UStmt' sizeT
elimHolesS UHoleS{} = error "cannot eliminate hole"
elimHolesS _ = error "TODO"
