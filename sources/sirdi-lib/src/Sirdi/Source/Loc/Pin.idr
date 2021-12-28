||| Types for pinning source locations to a specific version.
module Sirdi.Source.Loc.Pin


||| Whether we know if the location is pinned to a specific version.
public export
data PinKind : Type where
    ||| We know the location is pinned.
    IsPinned : PinKind

    ||| We are not sure whether the location is pinned or not.
    |||
    ||| This is useful when we read a location from a configuration file, as
    ||| the user can optionally supply a pin themselves.
    MaybePinned : PinKind


||| Specifies the version of a source.
||| @ pk The kind of pin
||| @ pinTy The type to store if the source is pinned.
public export
Pin : (pk : PinKind) -> (pinTy : Type) -> Type
Pin IsPinned    a = a
Pin MaybePinned a = Maybe a
