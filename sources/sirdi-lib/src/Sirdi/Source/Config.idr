module Sirdi.Source.Config

import Sirdi.Source.Loc
import Sirdi.Source.Files
import Sirdi.Package.Recipe
import Sirdi.Package.Recipe.Parse
import Sirdi.Package.Identifier
import Util.IOEither
import Util.Files
import System.File.ReadWrite
import Util.TOML
import Decidable.Equality
import Data.List
import Util.List


public export
record Config (0 loc : Loc IsPinned) where
    constructor MkConfig
    pkgs : List (name ** Recipe (Normal name loc))


readConfig : Files loc -> IOEither String String
readConfig files =
    let fp = files.dir /> "sirdi.toml"
    in embed $ readFile $ show fp


FromTOML (Config loc) where
    fromTOML (VTable x) = MkConfig <$> traverse (\(name, v) => ?h1) (SortedMap.toList x)
    fromTOML _ = Left "Expected config file to be a table"


export
getConfig : Files loc -> IOEither String (Config loc)
getConfig files = do
    contents <- readConfig files
    toml <- embed $ pure $ parseTOML contents
    embed $ pure $ fromTOML (VTable toml)


export
getPkgDesc : (name : String) -> Files loc -> IOEither String (Recipe (Normal name loc))
getPkgDesc name files = do
    cfg <- getConfig files
    case dLookup name cfg.pkgs of
         Just x => pure x
         Nothing => throw "Package \{name} is not defined."
