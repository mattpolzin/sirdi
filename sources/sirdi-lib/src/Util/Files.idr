module Util.Files


public export
record FilePath where
    constructor MkFilePath
    inner : String
