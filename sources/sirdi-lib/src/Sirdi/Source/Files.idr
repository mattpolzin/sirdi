||| Types for dealing with downloaded source files.
module Sirdi.Source.Files

import Data.Hashable
import Sirdi.Source.Loc
import Sirdi.Directories
import System.Directory
import Util.Git
import Util.Files
import Util.IOEither


||| The downloaded source files for a given source.
export
record Files (0 for : Loc IsPinned) where
    constructor MkFiles
    filepath : Path


export
(.dir) : Files loc -> Path
(.dir) = (.filepath)


directory : Loc IsPinned -> Path
directory loc = sirdiSources /> show (hash loc)


||| Try and fetch the files from a source, without checking whether they
||| already exist.
doFetchSource : (loc : Loc IsPinned) -> IOEither String (Files loc)
doFetchSource loc@(Local filepath) = copyDirRec filepath (directory loc) $> MkFiles (directory loc)
doFetchSource loc@(Git url commit) = gitClone url commit (directory loc) $> MkFiles (directory loc)


||| Fetch the files from a source if they have not already been fetched.
export
fetchSource : (loc : Loc IsPinned) -> IOEither String (Files loc)
fetchSource loc = do
    alreadyFetched <- exists (show $ directory loc)
    if alreadyFetched
        then pure $ MkFiles $ directory loc
        else doFetchSource loc
