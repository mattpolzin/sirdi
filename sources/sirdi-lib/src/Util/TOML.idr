module Util.TOML

import public Language.TOML


public export
interface FromTOML a where
    fromTOML : Value -> Either String a


export
tryLookup : String -> Value -> Either String Value
tryLookup name (VTable x) =
    case lookup name x of
         Nothing => Left "Unable to find key \{name}"
         (Just v) => Right v
tryLookup name _ = Left "Expected a table with the key \{name} defined"
