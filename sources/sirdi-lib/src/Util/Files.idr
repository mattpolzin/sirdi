module Util.Files

import public System.Path
import Util.IOEither
import System
import System.Directory


public export
newDir : Path -> IOEither String ()
newDir path = mapErr show $ MkEitherT $ createDir $ show path


public export
newTempDir : IOEither String Path
newTempDir = do
    (out, n) <- run "mktemp -d"
    case n of
         0 => pure $ parse out
         _ => throw "Failed to create temp dir"


public export
copyDirRec : Path -> Path -> IOEither String ()
copyDirRec from to = do
    n <- system "cp -r \{show from} \{show to}"
    case n of
         0 => pure ()
         _ => throw "Failed to recursively copy \{show from} to \{show to}"


public export
symLink : Path -> Path -> IOEither String ()
symLink from to = do
    n <- system "ln -s \{show from} \{show to}"
    case n of
         0 => pure ()
         _ => throw "Failed to symlink \{show from} to \{show to}"

