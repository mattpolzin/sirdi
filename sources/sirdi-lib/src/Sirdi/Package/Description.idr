module Sirdi.Package.Description

import Sirdi.Package.Identifier


public export
data ExtraOpts : PackageKind -> Type where
    LibOpts : (modules : List String) -> ExtraOpts Library
    AppOpts : (main : String ) -> ExtraOpts Application


public export
record Description (for : PkgID pk) where
    constructor MkDescription
    deps     : List (PkgID Library)
    passthru : List (String, String)
    extra    : ExtraOpts pk
