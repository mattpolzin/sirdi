module Util.IOEither

import public Control.Monad.Either


public export
IOEither : Type -> Type -> Type
IOEither err = EitherT err IO


public export
throw : e -> IOEither e a
throw = MkEitherT . pure . Left


public export
mapErr : (e -> e') -> IOEither e a -> IOEither e' a
mapErr f (MkEitherT x) = MkEitherT $ bimap f id <$> x
