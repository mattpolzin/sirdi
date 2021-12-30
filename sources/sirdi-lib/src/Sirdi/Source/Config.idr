module Sirdi.Source.Config

import Sirdi.Source.Loc
import Sirdi.Source.Files
import Sirdi.Package.Description
import Sirdi.Package.Description.Parse
import Sirdi.Package.Identifier
import Util.IOEither
import Util.Files
import System.File.ReadWrite
import Util.TOML


public export
record Config (loc : Loc IsPinned) where
    constructor MkConfig
    pkgs : List (name ** pk ** Description $ Normal pk name loc)


readConfig : {loc : _} -> Files loc -> IOEither String String
readConfig files = mapErr show $ MkEitherT $ readFile "\{files.dir.inner}/sirdi.toml"


FromTOML (Config loc) where
    fromTOML {loc} (VTable x) = do
        pkgs <- traverse (\(name, v) => do
            (pk ** desc) <- descFromTOML {name = name} {loc = loc} v
            pure $ the (name ** pk ** Description $ Normal pk name loc) $ MkDPair name $ MkDPair pk $ desc
              ) (SortedMap.toList x)
        pure $ MkConfig pkgs
    fromTOML _ = Left "Expected config file to be a table"


export
getConfig : {loc : _} -> Files loc -> IOEither String (Config loc)
getConfig files = do
    contents <- readConfig files
    toml <- mapErr show $ MkEitherT $ pure $ parseTOML contents
    MkEitherT $ pure $ fromTOML (VTable toml)
