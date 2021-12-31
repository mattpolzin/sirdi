module Sirdi.Package.Description

import Sirdi.Package.Identifier
import Sirdi.Source.Loc
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
record Description where
    constructor MkDescription
    kind     : PackageKind
    deps     : List Package
    extra    : case kind of { Library => LibOpts; Application => AppOpts }
    passthru : List (String, String)


public export
data Recipe : PackageKind -> Package -> Type where
    NormalRecipe : (desc : Description) -> Recipe desc.kind (Normal name loc)
    InstalledRecipe : Recipe Library (Installed name)
