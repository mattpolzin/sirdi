||| Types for specifying the source location (git, local, etc.) of a package.
module Sirdi.Source.Loc

import Sirdi.Source.Loc.Pin
import Util.IOEither
import Util.Git
import Util.URL
import Util.Files


||| The location of some source files (potentially containing many packages).
public export
data Loc : PinKind -> Type where
    ||| The files are located on a repote git repository.
    Git : URL -> Pin sk CommitHash -> Loc sk

    ||| The files are located in a directory on the local mcahine.
    Local : FilePath -> Loc sk


||| Take a source which may or may not be pinned, and pin it. This involves
||| inferring a suitable version to use.
export
pinLoc : Loc MaybePinned -> IOEither String (Loc IsPinned)
pinLoc (Local fp)         = pure $ Local fp
pinLoc (Git url (Just x)) = pure $ Git url x
pinLoc (Git url Nothing)  = Git url <$> gitRemoteLatestCommitHash url
