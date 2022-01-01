module Util.List

import Decidable.Equality


export
dLookup : DecEq a => (e : a) -> List (DPair a b) -> Maybe (b e)
dLookup e [] = Nothing
dLookup e (MkDPair x p :: xs) with (decEq e x)
  dLookup e (MkDPair e p :: xs) | Yes Refl = Just p
  dLookup e (MkDPair x p :: xs) | No neq   = dLookup e xs
