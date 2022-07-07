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
boolEqToEq : (a : JType) -> (b : JType) -> (a == b = True) -> a = b
boolEqToEq JBool JBool prf = Refl
boolEqToEq JBool JInt prf = absurd prf
boolEqToEq JInt JBool prf = absurd prf
boolEqToEq JInt JInt prf = Refl

public export
typeMatchProof : (k : Fin n) -> (vars : Variables n) -> (jty : JType) -> (getType k vars == jty = True) -> IsOfType n k jty vars
typeMatchProof k vars jty prf = let varTy = getType k vars in
                                    eqToProof k vars jty (boolEqToEq _ _ prf)

public export
DecEq JType where
  decEq JBool JBool = Yes Refl
  decEq JBool JInt = No notJBoolEqJInt
  decEq JInt JInt = Yes Refl
  decEq JInt JBool = No (\prf => notJBoolEqJInt (sym prf))

