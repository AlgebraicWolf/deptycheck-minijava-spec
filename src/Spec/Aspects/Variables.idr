module Spec.Aspects.Variables

import Data.Fin

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

export
Eq Identifier where
  (MkIdentifier x) == (MkIdentifier y) = x == y

--- Non-polymorphic variable storage ---
--- At this stage variables do not have a name, since we can assign them later
public export
data Variables : Nat -> Type where
  Nil : Variables Z
  (::) : JType -> Variables n -> Variables (S n)

%name Variables vars

public export
data IsOfType : (n : Nat) -> (k : Fin n) -> (jty : JType) -> (vars : Variables n) -> Type where
  Here : IsOfType (S n) FZ jty (jty::vars)
  There : IsOfType n k jty vars -> IsOfType (S n) (FS k) jty (jty'::vars)

public export
getType : Fin n -> Variables n -> JType
getType FZ (x :: vars) = x
getType (FS n) (y :: vars) = getType n vars


