module QCompose.Majority (majority) where

import QCompose.Basic (Unitary)
import Quipper (Qubit)

majority :: [Unitary (qa, Qubit)] -> Unitary (qa, Qubit, [qa], [Qubit])
majority us = \(a, f, as, fs) -> do
    undefined

