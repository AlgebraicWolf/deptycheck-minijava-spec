module Spec.Aspects.Variables

import public Spec.Aspects.Types

%default total

export
data Identifier = MkIdentifier String

%name Identifier n, m

export
FromString Identifier where
  fromString = MkIdentifier

export
Show Identifier where
  show (MkIdentifier n) = n


--- Non-polymorphic variable storage ---
public export
data Variables : Type where
  Nil : Variables
  (::) : (Identifier, JType) -> Variables -> Variables

%name Variables vars

--- Lookup for non-polymorphic version of Variables ---
public export
data Lookup : Identifier -> Variables -> Type where
  Here : (y : JType) -> Lookup x $ (x, y)::vars
  There : Lookup x vars -> Lookup x $ (x', y')::vars

public export
reveal : Lookup n vars -> JType
reveal (Here y) = y
reveal (There subl) = reveal subl

public export
(.reveal) : Lookup n vars -> JType
(.reveal) = reveal

public export
Uninhabited (Lookup n []) where
  uninhabited (Here _) impossible
  uninhabited (There p) impossible
