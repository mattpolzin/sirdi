module Util.IOEither

import public Control.Monad.Either


public export
IOEither : Type -> Type -> Type
IOEither err = EitherT err IO


public export
throw : e -> IOEither e a
throw = MkEitherT . pure . Left
