module Util.Git

import Util.IOEither
import Util.URL
import Util.Files


public export
record CommitHash where
    constructor MkCommitHash
    inner : String


public export
gitRemoteLatestCommitHash : URL -> IOEither String CommitHash
gitRemoteLatestCommitHash url = ?gitRemoteLatestCommitHash_rhs


public export
gitClone : URL -> CommitHash -> FilePath -> IOEither String ()
gitClone x y z = ?gitClone_rhs
