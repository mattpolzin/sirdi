module Sirdi.Directories

import Util.Files


export
sirdiRoot : FilePath
sirdiRoot = MkFilePath ".sirdi"


export
sirdiSources : FilePath
sirdiSources = MkFilePath "\{sirdiRoot.inner}/sources"


export
sirdiOutputs : FilePath
sirdiOutputs = MkFilePath "\{sirdiRoot.inner}/outputs"
