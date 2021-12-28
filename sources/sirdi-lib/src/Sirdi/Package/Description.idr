module Sirdi.Package.Description

import Sirdi.Package.Identifier


public export
record LibraryDescription where
    constructor MkLibraryDescription
    modules : List String


public export
record ApplicationDescription where
    constructor MkApplicationDescription
    main : String


public export
record Description {pk : _} (for : PkgID pk) where
    constructor MkDescription
    deps     : List (PkgID Library)
    passthru : List (String, String)

    opts : case pk of
                Library => LibraryDescription
                Application => ApplicationDescription
