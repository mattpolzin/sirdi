module Util.Ipkg

import Data.List
import Data.String
import System.File.ReadWrite
import Util.IOEither


public export
record Ipkg where
    constructor MkIpkg
    name : String
    depends : List String
    modules : List String
    main : Maybe String
    exec : Maybe String
    passthru : List (String, String)


Show Ipkg where
    show p = let depends = if p.depends == [] then "" else "depends = \{concat $ intersperse ", " p.depends}"
                 mains = case p.main of { Just s => "main = \{s}"; Nothing => "" }
                 exec = case p.exec of { Just s => "executable = \{s}"; Nothing => "" }
                 passthru = fastUnlines $ map (\(k, s) => "\{k} = \"\{s}\"" ) p.passthru
      in """
package \{p.name}
sourcedir = "src"
modules = \{concat $ intersperse ", " p.modules}
\{depends}
\{mains}
\{exec}
\{passthru}
"""


public export
writeIpkg : Ipkg -> String -> IOEither String ()
writeIpkg ipkg dest = mapErr show $ MkEitherT $ writeFile dest (show ipkg)
