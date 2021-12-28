module Util.IOEither

import public Control.Monad.Either


public export
IOEither : Type -> Type -> Type
IOEither err = EitherT err IO
