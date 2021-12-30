module Sirdi.Package.Description

import Sirdi.Package.Identifier
import Util.The


public export
record LibOpts where
    constructor MkLibOpts
    modules : List String


public export
record AppOpts where
    constructor MkAppOpts
    main : String


public export
record Description {pk : _} (for : PkgID pk) where
    constructor MkDescription
    kind     : The pk
    deps     : List (PkgID Library)
    extra    : case pk of { Library => LibOpts; Application => AppOpts }
    passthru : List (String, String)
