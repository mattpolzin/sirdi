module Sirdi.Package.Output

import Sirdi.Package.Identifier
import Sirdi.Package.Recipe
import Sirdi.Source.Loc
import Sirdi.Source.Files
import Sirdi.Directories
import Data.List.Quantifiers
import Util.Files
import Util.IOEither
import Util.Quantifiers
import Util.Ipkg
import Data.Hashable
import System


export
record Output (pkg : Package) where
    constructor MkOutput
    path : Path


export
buildInstalled : Output (Installed name)
buildInstalled = MkOutput emptyPath


dirName : String -> Loc IsPinned -> String
dirName name loc = "\{name}\{show $ hash loc}"


linkDep : Path -> DPair Package Output -> IOEither String ()
linkDep depDir (Installed name ** output) = pure ()
linkDep depDir (Normal name loc ** output) = symLink output.path $ depDir /> dirName name loc


recipeToIpkg : Recipe pkg -> String -> Ipkg
recipeToIpkg (MkRecipe kind deps extra passthru) name =
    MkIpkg {
        name = name,
        depends = map (.name) deps,
        modules = case kind of { Library => extra.modules; Application => [] },
        main = case kind of { Library => Nothing; Application => Just extra.main },
        exec = case kind of { Library => Nothing; Application => Just "main" },
        passthru = passthru
    }


export
buildNormal : {name : _}
           -> {loc : _}
           -> (recipe : Recipe (Normal name loc))
           -> Files loc
           -> All Output recipe.deps
           -> IOEither String (Output (Normal name loc))
buildNormal recipe files depOutputs =
    withTempDir $ \tmp => do
        
        let depDir = tmp /> "depends"

        newDir depDir
        copyDirRec files.dir tmp

        traverse_ (linkDep depDir) (allToList depOutputs)

        let ipkg = recipeToIpkg recipe name
        let ipkgPath = show $ tmp /> name <.> "ipkg"

        writeIpkg ipkg ipkgPath
        _ <- system "idris2 --build \{ipkgPath}"

        let buildDir = tmp /> "build"
        let outDir = sirdiOutputs /> dirName name loc

        case recipe.kind of
             Library => do
                 copyDirRec (buildDir /> "ttc") outDir
                 pure $ MkOutput outDir

             Application => do
                 newDir outDir
                 copyFileInto (buildDir /> "exec" /> "main") outDir
                 pure $ MkOutput $ outDir /> "main"
