||| Types for uniquely identifying a package through its source location
||| and name.
module Sirdi.Package.Identifier

import Sirdi.Source.Loc
import Sirdi.Source.Loc.Pin


||| Whether a package is a library or application.
public export
data PackageKind : Type where
    ||| The package is a library.
    Library     : PackageKind

    ||| The package is an application (i.e. produces an executable).
    Application : PackageKind


||| A unique package identifier describing where to find a package. This type
||| is primarily used in describing where to find a dependency.
public export
data PkgID : PackageKind -> Type where
    Normal : (pk : _) -> (name : String) -> (loc : Loc IsPinned) -> PkgID pk
    Legacy : (name : String) -> PkgID Library


public export
(.name) : PkgID pk -> String
(.name) (Normal _ name _) = name
(.name) (Legacy name)     = name
