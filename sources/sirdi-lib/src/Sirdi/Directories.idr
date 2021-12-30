module Sirdi.Directories

import public System.Path


export
sirdiRoot : Path
sirdiRoot = parse ".sirdi"


export
sirdiSources : Path
sirdiSources = sirdiRoot /> "sources"


export
sirdiOutputs : Path
sirdiOutputs = sirdiRoot /> "outputs"
