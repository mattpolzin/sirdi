module Util.Git

import Util.IOEither
import Util.URL


public export
record CommitHash where
    constructor MkCommitHash
    inner : String


public export
gitRemoteLatestCommitHash : URL -> IOEither String CommitHash
gitRemoteLatestCommitHash url = ?gitRemoteLatestCommitHash_rhs
