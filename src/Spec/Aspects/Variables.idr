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
data Variable : Type where
  MkVar : JType -> Bool -> Variable

public export
typeOf : Variable -> JType
typeOf (MkVar ty _) = ty

public export
initOf : Variable -> Bool
initOf (MkVar _ usage) = usage

public export
data Variables : Type where
  Nil : Variables
  (::) : Variable -> Variables -> Variables

%name Variables vars

public export
data IsOfType : (k : Nat) -> (jty : JType) -> (vars : Variables) -> Type where
  THere : IsOfType Z jty ((MkVar jty usage)::vars)
  TThere : IsOfType k jty vars -> IsOfType (S k) jty (var::vars)

public export
data IsInit : (k : Nat) -> (vars : Variables) -> Type where
  IHere : IsInit Z ((MkVar jty True)::vars)
  IThere : IsInit k vars -> IsInit (S k) (var::vars)

public export
length : Variables -> Nat
length [] = Z
length (_::vars) = S $ length vars

public export
getType : (vars : Variables) -> (k : Fin $ length vars) -> JType
getType ((MkVar jty _) :: vars) FZ = jty
getType (_ :: vars) (FS k) = getType vars k

public export
getInit : (vars : Variables) -> (k : Fin $ length vars) -> Bool
getInit ((MkVar _ u) :: vars) FZ = u
getInit (_ :: vars) (FS k) = getInit vars k

public export
makeInit : (vars : Variables) -> (k : Fin $ length vars) -> Variables
makeInit ((MkVar jty _) :: vars) FZ = (MkVar jty True) :: vars
makeInit (x :: vars) (FS y) = x :: makeInit vars y

public export
DecEq JType where
  decEq JBool JBool = Yes Refl
  decEq JBool JInt = No (\prf => case prf of Refl impossible)

  decEq JInt JInt = Yes Refl
  decEq JInt JBool = No (\prf => case prf of Refl impossible)

