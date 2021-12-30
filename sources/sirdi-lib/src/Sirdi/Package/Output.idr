||| Types relating to the build outputs of packages.
module Sirdi.Package.Output

import Sirdi.Package.Identifier
import Sirdi.Directories
import Sirdi.Source.Loc
import Util.Files
import Util.IOEither
import Data.Hashable
import System


||| The TTC files produced as a result of building the specified library.
export
data TTCFiles : PkgID Library -> Type where
    MkTTCFiles : (name : _) -> (loc : _) -> TTCFiles (Normal Library name loc)


||| The result executable from building the specified application.
export
data Executable : PkgID Application -> Type where
    MkExecutable : (name : _) -> (loc : _) -> Executable (Normal Application name loc)


||| Computes a type to represent the output for a package.
public export
Output : {pk : PackageKind} -> PkgID pk -> Type
Output {pk = Library} pkg@(Normal _ _ _) = TTCFiles pkg
Output {pk = Library} pkg@(Legacy _)     = ()
Output {pk = Application} pkg            = Executable pkg


directory : String -> Loc IsPinned -> Path
directory name loc = sirdiOutputs /> "\{name}\{show $ hash loc}"


||| Get the directory where the TTC files are being stored.
public export
(.dir) : TTCFiles pkg -> Path
(.dir) (MkTTCFiles name loc) = directory name loc


||| Get the filepath to the executable.
public export
(.file) : Executable pkg -> Path
(.file) (MkExecutable name loc) = directory name loc /> "main"


||| Given the executable for a package, run it.
export
runExecutable : Executable pkg -> IOEither String String
runExecutable exec = do
    (output, n) <- run "\{show exec.file}"
    case n of
        0 => pure output
        _ => throw "Failed to run executable \{show exec.file} with output:\n\{output}"
