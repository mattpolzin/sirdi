module Util.Files

import public System.Path
import Util.IOEither
import System


{-public export
record FilePath where
    constructor MkFilePath
    inner : String-}


public export
newDir : Path -> IOEither String ()


public export
newTempDir : IOEither String Path
newTempDir = do
    (out, n) <- run "mktemp -d"
    case n of
         0 => pure $ parse out
         _ => throw "Failed to create temp dir"


public export
copyDirRec : Path -> Path -> IOEither String ()
copyDirRec x y = ?copyDirRec_rhs


public export
symLink : Path -> Path -> IOEither String ()
symLink x y = ?symLink_rhs
