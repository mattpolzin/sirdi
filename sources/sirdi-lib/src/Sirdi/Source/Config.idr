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
import Decidable.Equality
import Data.List


public export
record Config (loc : Loc IsPinned) where
    constructor MkConfig
    pkgs : List (String, Description)


readConfig : {loc : _} -> Files loc -> IOEither String String
readConfig files = mapErr show $ MkEitherT $ readFile $ show $ files.dir /> "sirdi.toml"


FromTOML (Config loc) where
    fromTOML {loc} (VTable x) = do
        pkgs <- traverse (\(name, v) => (name,) <$> descFromTOML v) (SortedMap.toList x)
        pure $ MkConfig pkgs
    fromTOML _ = Left "Expected config file to be a table"


export
getConfig : {loc : _} -> Files loc -> IOEither String (Config loc)
getConfig files = do
    contents <- readConfig files
    toml <- mapErr show $ MkEitherT $ pure $ parseTOML contents
    MkEitherT $ pure $ fromTOML (VTable toml)


export
getPkgDesc : {loc : _} -> (name : String) -> Files loc -> IOEither String Description
getPkgDesc name files = do
    cfg <- getConfig files
    case Data.List.lookup name cfg.pkgs of
         Just x => pure x
         Nothing => throw "Package \{name} is not defined."
