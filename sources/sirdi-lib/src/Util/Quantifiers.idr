module Util.Quantifiers

import Data.List.Quantifiers


public export
allToList : {xs : List a} -> All p xs -> List (DPair a p)
allToList []        = []
allToList (x :: xs) = (_ ** x) :: allToList xs


export
forAllM : Monad m => ((x : a) -> m (p x)) -> (xs : List a) -> m (All p xs)
forAllM f [] = pure []
forAllM f (x :: xs) = [| f x :: forAllM f xs |]
