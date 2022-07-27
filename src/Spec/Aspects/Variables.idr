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
data Variables : Nat -> Type where
  Nil : Variables Z
  (::) : Variable -> Variables n -> Variables (S n)

%name Variables vars

public export
data IsOfType : (n : Nat) -> (k : Fin n) -> (jty : JType) -> (vars : Variables n) -> Type where
  THere : IsOfType (S n) FZ jty ((MkVar jty usage)::vars)
  TThere : IsOfType n k jty vars -> IsOfType (S n) (FS k) jty (var::vars)

public export
data IsInit : (n : Nat) -> (k : Fin n) -> (vars : Variables n) -> Type where
  IHere : IsInit (S n) FZ ((MkVar jty True)::vars)
  IThere : IsInit n k vars -> IsInit (S n) (FS k) (var::vars)

public export
getType : Fin n -> Variables n -> JType
getType FZ (var :: vars) = typeOf var
getType (FS n) (y :: vars) = getType n vars

public export
getInit : Fin n -> Variables n -> Bool
getInit FZ (var :: vars) = initOf var
getInit (FS n) (y :: vars) = getInit n vars

public export
makeInit : Fin n -> Variables n -> Variables n
makeInit FZ ((MkVar jty _) :: vars) = (MkVar jty True) :: vars
makeInit (FS n) (y :: vars) = y :: makeInit n vars

public export
DecEq JType where
  decEq JBool JBool = Yes Refl
  decEq JBool JInt = No (\prf => case prf of Refl impossible)

  decEq JInt JInt = Yes Refl
  decEq JInt JBool = No (\prf => case prf of Refl impossible)

