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


linkDep : {dep : Package} -> {recipe : Recipe Library dep}
       -> Output recipe -> Path -> IOEither String String
linkDep {dep = (Normal name loc)} {recipe = NormalRecipe (MkDescription Library deps extra passthru)} ttcFiles dependsDir = do
    let installedAs = "\{name}\{show $ hash loc}"
    symLink ttcFiles.dir (dependsDir /> installedAs)
    pure installedAs
linkDep {dep = (Installed name)} {recipe = InstalledRecipe} () _ = pure name


export
runRecipe : {pkg : Package} -> (recipe : Recipe pk pkg) -> Input recipe
         -> IOEither String (Output recipe)
runRecipe {pkg = (Installed name)} InstalledRecipe () = pure ()
runRecipe {pkg = (Normal name loc)} (NormalRecipe (MkDescription kind deps extra passthru)) input = do
    let installDir = sirdiOutputs /> "\{name}\{show $ hash loc}"
    alreadyBuilt <- exists $ show installDir
    if alreadyBuilt
        then case kind of
                  Library => pure $ MkTTCFiles installDir
                  Application => pure $ MkExecutable $ installDir /> "main"
        else do
    tempDir <- newTempDir
    let dependsDir = tempDir /> "depends"
    mapErr show $ MkEitherT $ createDir $ show dependsDir

    -- Copy source files into the build dir.
    copyDirRec input.sourceFiles.dir tempDir

    -- Link built dependencies' TTC files to the build dir.
    depNames <- traverse (\((_ ** _) ** depOutput) => linkDep depOutput dependsDir) $ allToList input.depOutputs

    -- Use the Ipkg system to build the package. TODO: Use the Idris API.
    let ipkg = MkIpkg {
      name = name,
      depends = depNames,
      modules = case kind of { Library => extra.modules; Application => [] },
      main = case kind of { Library => Nothing; Application => Just extra.main },
      exec = case kind of { Library => Nothing; Application => Just "main" },
      passthru = passthru }

    let ipkgPath = tempDir /> name <.> ".ipkg"
    writeIpkg ipkg (show ipkgPath)
    _ <- system "idris2 --build \{show ipkgPath}"

    let installDir = sirdiOutputs /> "\{name}\{show $ hash loc}"

    case kind of
         Library => do
            let ttcDir = (installDir /> "build") /> "ttc"
            copyDirRec ttcDir installDir
            pure $ MkTTCFiles ttcDir
         Application => do
            newDir installDir
            mapErr show $ MkEitherT $ copyFile "\{show tempDir}/build/exec/main" "\{show installDir}/main"
            pure $ MkExecutable $ installDir /> "main"
