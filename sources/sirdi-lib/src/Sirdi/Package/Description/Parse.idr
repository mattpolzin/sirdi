module Sirdi.Package.Description.Parse

import Sirdi.Package.Description
import Sirdi.Package.Identifier
import Sirdi.Source.Loc
import Util.TOML
import Util.URL
import Util.Files
import Util.Git
import Util.The

{-
[sirdi]
main = "Main"

[sirdi.dependencies]
hashable = { git = "z-snails/idris2-hashable", commit = "fefc23321f2c23f1" }
collie = { git = "ohad/collie", commit = "fefc23321f2c23f1" }


[sirdi-lib]
modules = [ "Sirdi.Something", "Sirdi.Other" ]

[sirdi-lib.dependencies]
hashable = { git = "z-snails/idris2-hashable", commit = "fefc23321f2c23f1" }
-}


FromTOML String where
    fromTOML (VString s) = Right s
    fromTOML _ = Left "Expected string"


FromTOML PackageKind where
    fromTOML (VString "application") = Right Application
    fromTOML (VString "library") = Right Library
    fromTOML (VString _) = Left "Expected 'application' or 'library' for the package type"
    fromTOML _ = Left "Expected a string for the package type"


FromTOML CommitHash where
    fromTOML (VString commit) = Right $ MkCommitHash commit
    fromTOML _ = Left "Expected commit hash as string"


FromTOML URL where
    fromTOML (VString url) = Right $ MkURL url
    fromTOML _ = Left "Expected url hash as string"


FromTOML FilePath where
    fromTOML (VString fp) = Right $ MkFilePath fp
    fromTOML _ = Left "Expected filepath as string"


FromTOML (Loc IsPinned) where
    fromTOML x = case git x of
                     Left err => local x
                     Right loc => Right loc
        where
            git : Value -> Either String (Loc IsPinned)
            git tbl = do
                url <- tryLookup "git" tbl >>= fromTOML {a = URL}
                commit <- tryLookup "commit" tbl >>= fromTOML {a = CommitHash}
                Right $ Git url commit

            local : Value -> Either String (Loc IsPinned)
            local tbl = do
                fp <- tryLookup "local" tbl >>= fromTOML {a = FilePath}
                Right $ Local fp

    fromTOML _ = Left "Expected a source location as a table"


FromTOML (List (PkgID Library)) where
    fromTOML (VTable x) = traverse (\(name, v) => Normal Library name <$> fromTOML v) $ SortedMap.toList x
    fromTOML _ = Left "Dependencies should be represented as a table"


modulesFromTOML : Value -> Either String (List String)
modulesFromTOML (VArray xs) = traverse (fromTOML {a = String}) xs
modulesFromTOML _ = Left "Expected modules to be array"


FromTOML (pk ** Description (Normal pk name loc)) where
    fromTOML desc = do
        kind <- tryLookup "type" desc >>= fromTOML {a = PackageKind}
        deps <- tryLookup "dependencies" desc >>= fromTOML {a = List (PkgID Library)}

        case kind of
             Library => do
                modules <- tryLookup "modules" desc >>= modulesFromTOML
                pure $ (Library ** MkDescription (Is Library) deps (MkLibOpts modules) [])
             Application => do
                main <- tryLookup "main" desc >>= fromTOML {a = String}
                pure $ (Application ** MkDescription (Is Application) deps (MkAppOpts main) [])


export
descFromTOML : Value -> Either String (pk ** Description (Normal pk name loc))
descFromTOML = fromTOML
