module Util.The


public export
data The : {0 a : Type} -> (0 x : a) -> Type where
    Is : {0 a : Type} -> (x : a) -> The {a} x
