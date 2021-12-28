module Sirdi.Package.Description

import Sirdi.Package.Identifier


public export
record Description (for : PkgID pk) where
    constructor MkDescription
    deps     : List (PkgID Library)
    modules  : List String
    main     : Maybe String
    passthru : List (String, String)


