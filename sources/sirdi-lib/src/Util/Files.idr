module Util.Files

import public System.Path
import Util.IOEither
import System
import System.Directory


public export
copyFileInto : Path -> Path -> IOEither String ()
copyFileInto file dir = do
    (out, n) <- run "cp \{show file} \{show dir}/"
    case n of
         0 => pure ()
         _ => throw "Failed to copy file into"


public export
newTempDir : IOEither String Path
newTempDir = do
    (out, n) <- run "mktemp -d"
    case n of
         0 => pure $ parse out
         _ => throw "Failed to create temp dir"


public export
newDir : Path -> IOEither String ()
newDir path = do
    (out, n) <- run "mkdir \{show path}"
    case n of
         0 => pure ()
         _ => throw "Failed to create dir \{show path}. Output:\n\{out}"


public export
removeDir : Path -> IOEither String ()
removeDir path = do
    (out, n) <- run "rm -rf \{show path}"
    case n of
         0 => pure ()
         _ => throw "Failed to remove dir \{show path}. Output:\n\{out}"


public export
withTempDir : (Path -> IOEither String a) -> IOEither String a
withTempDir f = do
    dir <- newTempDir
    res <- f dir
    removeDir dir
    pure res


public export
copyDirRec : Path -> Path -> IOEither String ()
copyDirRec from to = do
    n <- system "cp -r \{show from} \{show to}"
    case n of
         0 => pure ()
         _ => throw "Failed to recursively copy \{show from} to \{show to}"


public export
symLink : Path -> Path -> IOEither String ()
symLink from to = do
    n <- system "ln -s \{show from} \{show to}"
    case n of
         0 => pure ()
         _ => throw "Failed to symlink \{show from} to \{show to}"

