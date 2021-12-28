module Sirdi.Package.Output.Build

import Sirdi.Package.Output
import Sirdi.Source.Files
import Sirdi.Source.Loc
import Sirdi.Package.Description
import Sirdi.Package.Identifier
import Data.List.Quantifiers
import Util.IOEither
import Util.Files
import Util.Quantifiers
import Util.Ipkg
import System.Directory


export
buildPackage : {pk : _}
            -> {name : _}
            -> Files loc
            -> (desc : Description (Normal pk name loc))
            -> All Output desc.deps
            -> IOEither String (Output (Normal pk name loc))
buildPackage {name} {pk} files desc depOutputs = do
    -- Create build directories.
    dir <- newTempDir
    mapErr show $ MkEitherT $ createDir "\{dir.inner}/depends"

    -- Write source files into the build dir.
    files.writeTo dir

    -- Write built dependencies' TTC files to the build dir.
    -- TODO: Store the TTC under a hash instead of package name, in case two
    --       packages have the same name.
    traverse_ (\(pkg ** ttcFiles) => ttcFiles.writeTo $ MkFilePath "\{dir.inner}/depends/\{pkg.name}") $ allToList depOutputs

    -- Use the Ipkg system to build the package. TODO: Use the Idris API.
    let ipkg = MkIpkg {
        name = name,
        depends = map (.name) desc.deps,
        modules = [],
        main = Nothing,
        exec = ?h2,
        passthru = desc.passthru
      }

    ?h1
