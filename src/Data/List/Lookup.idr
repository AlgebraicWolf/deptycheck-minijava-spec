module Data.List.Lookup

%default total

public export
data Lookup : a -> List (a, b) -> Type where
  Here : (y : b) -> Lookup x $ (x, y)::xys
  There : Lookup z xys -> Lookup z $ (x, y)::xys

public export
reveal : Lookup {b} x xys -> b
reveal (Here y) = y
reveal (There subl) = reveal subl

public export
(.reveal) : Lookup {b} x xys -> b
(.reveal) = reveal

public export
Uninhabited (Lookup {a} {b} x []) where
  uninhabited (Here _) impossible
  uninhabited (There p) impossible
