module Util.Quantifiers

import Data.List.Quantifiers


public export
allToList : {xs : List a} -> All p xs -> List (DPair a p)
allToList []        = []
allToList (x :: xs) = (_ ** x) :: allToList xs
