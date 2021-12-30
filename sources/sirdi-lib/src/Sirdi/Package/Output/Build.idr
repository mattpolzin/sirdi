module Sirdi.Package.Output.Build

import Sirdi.Package.Output
import Sirdi.Source.Files
import Sirdi.Source.Loc
import Sirdi.Package.Description
import Sirdi.Package.Identifier
import Sirdi.Directories
import Data.List.Quantifiers
import Util.IOEither
import Util.Files
import Util.Quantifiers
import Util.Ipkg
import System.Directory
import Data.Hashable
import System


doBuildPackage : {pk : _}
            -> {name : _}
            -> {loc : _}
            -> Files loc
            -> (desc : Description (Normal pk name loc))
            -> All Output desc.deps
            -> IOEither String (Output (Normal pk name loc))
doBuildPackage {pk} {name} {loc} files desc depOutputs = do
    -- Create build directories.
    tempDir <- newTempDir
    let dependsDir = tempDir /> "depends"
    mapErr show $ MkEitherT $ createDir $ show dependsDir

    -- Copy source files into the build dir.
    copyDirRec files.dir tempDir

    -- Link built dependencies' TTC files to the build dir.
    depNames <- traverse (\(pkg ** ttcFiles) => case pkg of
                                          (Legacy name) => pure name
                                          (Normal Library name loc) => do
                                              let installedAs = "\{name}\{show $ hash loc}"
                                              symLink ttcFiles.dir (dependsDir /> installedAs)
                                              pure installedAs
                                          ) $ allToList depOutputs

    -- Use the Ipkg system to build the package. TODO: Use the Idris API.
    let ipkg = MkIpkg {
        name = name,
        depends = depNames,
        modules = case pk of { Library => desc.extra.modules; Application => [] },
        main = case pk of { Library => Nothing; Application => Just desc.extra.main },
        exec = case pk of { Library => Nothing; Application => Just "main" },
        passthru = desc.passthru
      }

    let ipkgPath = tempDir /> name <.> ".ipkg"
    writeIpkg ipkg (show ipkgPath)
    _ <- system "idris2 --build \{show ipkgPath}"

    let installDir = directory name loc

    -- TODO: Cleanup temp dir. It would be nice if we had a "withTempDir" function in Util.
    case pk of
         Library     => do
            let ttcDir = (installDir /> "build") /> "ttc"
            copyDirRec ttcDir installDir
            pure $ MkTTCFiles name loc

         Application => do
            newDir installDir
            mapErr show $ MkEitherT $ copyFile "\{show tempDir}/build/exec/main" "\{show installDir}/main"
            pure $ MkExecutable name loc


export
buildPackage : {pk : _}
            -> {name : _}
            -> {loc : _}
            -> Files loc
            -> (desc : Description (Normal pk name loc))
            -> All Output desc.deps
            -> IOEither String (Output (Normal pk name loc))
buildPackage {pk} {name} {loc} files desc depOutputs = do
    alreadyBuilt <- exists $ show $ directory name loc
    if alreadyBuilt
        then case pk of
                  Library => pure $ MkTTCFiles name loc
                  Application => pure $ MkExecutable name loc
        else doBuildPackage files desc depOutputs
