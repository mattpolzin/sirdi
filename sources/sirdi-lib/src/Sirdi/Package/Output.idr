||| Types relating to the build outputs of packages.
module Sirdi.Package.Output

import Sirdi.Package.Identifier
import Util.Files
import Util.IOEither
import System


||| The TTC files produced as a result of building the specified library.
export
record TTCFiles (pkg : PkgID Library) where
    constructor MkTTCFiles
    dir : FilePath


||| The result executable from building the specified application.
export
record Executable (pkg : PkgID Application) where
    constructor MkExecutable
    file : FilePath


||| Computes a type to represent the output for a package.
public export
Output : {pk : PackageKind} -> PkgID pk -> Type
Output {pk = Library}     pkg = TTCFiles   pkg
Output {pk = Application} pkg = Executable pkg


||| Given the executable for a package, execute it.
export
runExecutable : Executable pkg -> IOEither String String
runExecutable exec = do
    (output, n) <- run "\{exec.file.inner}"
    case n of
        0 => pure output
        _ => throw "Failed to run executable \{exec.file.inner} with output:\n\{output}"


||| Write the TTC files to a specified directory.
export
(.writeTo) : TTCFiles pkg -> FilePath -> IOEither String ()
(.writeTo) (MkTTCFiles src) dest = copyDirRec src dest
