module Sirdi.Operations

import Sirdi.Package.Identifier
import Sirdi.Package.Description
import Sirdi.Package.Output
import Sirdi.Package.Output.Build
import Sirdi.Source.Loc
import Sirdi.Source.Files
import Util.IOEither
import Data.List.Quantifiers


mutual
    buildDeps : (deps : List (PkgID Library)) -> IOEither String (All Output deps)
    buildDeps [] = pure []
    buildDeps (dep :: deps) = [| recBuild dep :: buildDeps deps |]


    export
    recBuild : (pkg : PkgID pk) -> IOEither String (Output pkg)
    recBuild (Legacy name) = pure ()
    recBuild (Normal pk name loc) = do
        sourceFiles <- fetchSource loc
        pkgDesc <- ?getPkgDesc sourceFiles
        depOutputs <- buildDeps pkgDesc.deps
        buildPackage sourceFiles pkgDesc depOutputs
