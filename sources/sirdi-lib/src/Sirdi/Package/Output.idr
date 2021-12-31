||| Types relating to the build outputs of packages.
module Sirdi.Package.Output

import Sirdi.Package.Identifier
import Sirdi.Package.Description
import Sirdi.Directories
import Sirdi.Source.Loc
import Sirdi.Source.Files
import Util.Files
import Util.IOEither
import Data.Hashable
import System
import Data.List.Quantifiers
import Util.Quantifiers


||| The TTC files produced as a result of building the specified library.
export
data TTCFiles : (0 pkg : Package) -> Type where
    MkTTCFiles : Path -> TTCFiles pkg


||| The result executable from building the specified application.
export
data Executable : (0 pkg : Package) -> Type where
    MkExecutable : Path -> Executable pkg


public export
Output : Recipe kind pkg -> Type
Output InstalledRecipe = ()
Output (NormalRecipe desc) with (desc.kind)
  _ | Library = TTCFiles pkg
  _ | Application = Executable pkg


public export
record NormalInput (loc : Loc IsPinned) (desc : Description) where
    constructor MkNormalInput
    sourceFiles : Files loc
    depRecipes : All (Recipe Library) desc.deps
    depOutputs : All (\(dep ** recipe) => Output recipe) (allToList depRecipes)


public export
Input : {pkg : Package} -> Recipe pk pkg -> Type
Input {pkg = Normal name loc} (NormalRecipe desc) = NormalInput loc desc
Input InstalledRecipe = ()


{-
directory : Package -> Path
directory pkg = sirdiOutputs /> "\{pkg.name}\{show $ hash pkg.loc}"-}


||| Get the directory where the TTC files are being stored.
public export
(.dir) : TTCFiles pkg -> Path
(.dir) (MkTTCFiles path) = path


||| Get the filepath to the executable.
public export
(.file) : Executable pkg -> Path
(.file) (MkExecutable path) = path


||| Given the executable for a package, run it.
export
runExecutable : Executable pkg -> IOEither String String
runExecutable exec = do
    (output, n) <- run "\{show exec.file}"
    case n of
        0 => pure output
        _ => throw "Failed to run executable \{show exec.file} with output:\n\{output}"
