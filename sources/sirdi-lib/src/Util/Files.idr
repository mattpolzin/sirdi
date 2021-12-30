module Util.Files

import Util.IOEither
import System


public export
record FilePath where
    constructor MkFilePath
    inner : String


public export
newDir : FilePath -> IOEither String ()


public export
newTempDir : IOEither String FilePath
newTempDir = do
    (out, n) <- run "mktemp -d"
    case n of
         0 => pure $ MkFilePath out
         _ => throw "Failed to create temp dir"


public export
copyDirRec : FilePath -> FilePath -> IOEither String ()


public export
symLink : FilePath -> FilePath -> IOEither String ()
