module Sirdi.Package.Recipe

import Sirdi.Package.Identifier
import Sirdi.Source.Loc


public export
record LibOpts where
    constructor MkLibOpts
    modules : List String


public export
record AppOpts where
    constructor MkAppOpts
    main : String


public export
record Recipe (pkg : Package) where
    constructor MkRecipe
    kind     : PackageKind
    deps     : List Package
    extra    : case kind of { Library => LibOpts; Application => AppOpts }
    passthru : List (String, String)
