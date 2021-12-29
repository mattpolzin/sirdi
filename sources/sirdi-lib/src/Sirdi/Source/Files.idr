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
record Files (for : Loc IsPinned) where
    constructor MkFiles


directory : Loc IsPinned -> FilePath
directory loc = MkFilePath "\{sirdiSources.inner}/\{show $ hash loc}"


export
(.dir) : {loc : _} -> (0 _ : Files loc) -> FilePath
(.dir) {loc} _ = directory loc


||| Try and fetch the files from a source, without checking whether they
||| already exist.
doFetchSource : (loc : Loc IsPinned) -> IOEither String (Files loc)
doFetchSource loc@(Local filepath) = copyDirRec filepath (directory loc) $> MkFiles
doFetchSource loc@(Git url commit) = gitClone url commit (directory loc) $> MkFiles


||| Fetch the files from a source if they have not already been fetched.
public export
fetchSource : (loc : Loc IsPinned) -> IOEither String (Files loc)
fetchSource loc = do
    alreadyFetched <- exists (directory loc).inner
    if alreadyFetched
        then pure MkFiles
        else doFetchSource loc
