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

public export
Variables : Type
Variables = List (Identifier, JType)

%name Variables vars
