||| Types for dealing with downloaded source files.
module Sirdi.Source.Files

import Sirdi.Source.Loc
import Util.Git
import Util.Files
import Util.IOEither


||| The downloaded source files for a given source.
export
record Files (for : Loc IsPinned) where
    constructor MkFiles
    dir : FilePath


||| Fetch the files from a source.
export
fetchFiles : (loc : Loc IsPinned) -> IOEither String (Files loc)
fetchFiles (Local filepath) = pure $ MkFiles filepath
fetchFiles (Git url commit) = do
    dir <- newTempDir
    gitClone url commit dir
    pure $ MkFiles dir


||| Write the downloaded source files to a specified directory.
export
writeTo : Files loc -> FilePath -> IOEither String ()
writeTo (MkFiles src) dest = copyDirRec src dest
