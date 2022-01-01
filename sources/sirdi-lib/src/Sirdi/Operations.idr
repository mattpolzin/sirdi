module Sirdi.Operations

import Sirdi.Package.Identifier
import Sirdi.Package.Recipe
import Sirdi.Package.Output
import Sirdi.Source.Loc
import Sirdi.Source.Files
import Sirdi.Source.Config
import Util.IOEither
import Util.Quantifiers
import Data.List.Quantifiers


export
recBuild : (pkg : Package) -> IOEither String (Output pkg)
recBuild (Installed name) = pure buildInstalled
recBuild (Normal name loc) = do
    sourceFiles <- fetchSource loc
    recipe <- getPkgDesc name sourceFiles
    depOutputs <- forAllM recBuild recipe.deps
    buildNormal recipe sourceFiles depOutputs
