module Spec.Aspects.Variables

import Decidable.Equality.Core
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

public export
eqToProof : (k : Fin n) -> (vars : Variables n) -> (jty : JType) -> (getType k vars = jty) -> IsOfType n k jty vars
eqToProof k [] jty prf impossible
eqToProof FZ (x :: vars) jty prf = rewrite prf in Here
eqToProof (FS y) (x :: vars) jty prf = There $ eqToProof y vars jty prf

public export
DecEq JType where
  decEq JBool JBool = Yes Refl
  decEq JBool JInt = No (\prf => case prf of Refl impossible)

  decEq JInt JInt = Yes Refl
  decEq JInt JBool = No (\prf => case prf of Refl impossible)

