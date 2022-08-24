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
-- We will use natural numbers as variable names to keep stuff simple
public export
data InitState : Type where
  NotInit : InitState
  Init : InitState

public export
data Variable : Type where
  MkVar : Nat -> JType -> InitState -> Variable

public export
data NatNotEqual : Nat -> Nat -> Type where
  SZ : NatNotEqual (S n) Z
  ZS : NatNotEqual Z (S n)
  NotEqImpSNotEq : NatNotEqual n m ->
                   NatNotEqual (S n) (S m)

public export
data NameDifferent : Nat -> Variable -> Type where
  DiffName : NatNotEqual n m -> NameDifferent n (MkVar m jty init)

public export
nameOf : Variable -> Nat
nameOf (MkVar name _ _) = name

public export
typeOf : Variable -> JType
typeOf (MkVar _ ty _) = ty

data Variables : Type

data NameDoesNotExist : Nat -> Variables -> Type

data VariableDoesNotExist : Variable -> Variables -> Type

public export
data Variables : Type where
  Nil : Variables
  (::) : (var : Variable) ->
         (vars : Variables) ->
         VariableDoesNotExist var vars =>
         Variables

public export
data NameDoesNotExist : Nat -> Variables -> Type where
  NoVars : NameDoesNotExist name []
  DeclDiff : NameDoesNotExist name vars ->
             NameDifferent name var ->
             (prf : VariableDoesNotExist var vars) =>
             NameDoesNotExist name (var::vars)

public export
data VariableDoesNotExist : Variable -> Variables -> Type where
  NameAvailable : NameDoesNotExist name vars ->
                  VariableDoesNotExist (MkVar name jty init) vars

%name Variables vars

%hint
public export
nonExistenceTransform : VariableDoesNotExist (MkVar name jty init) vars -> VariableDoesNotExist (MkVar name jty init') vars
nonExistenceTransform (NameAvailable prf) = NameAvailable prf


-- Proof that there exists variable of certain type with certain name
public export
data ExistsOfType : (name : Nat) -> (jty : JType) -> (vars : Variables) -> Type where
  THere : (prf : NameDoesNotExist name vars) =>
          ExistsOfType name jty ((MkVar name jty init)::vars)
  TThere : ExistsOfType name jty vars ->
           (prf : VariableDoesNotExist var vars) =>
           ExistsOfType name jty (var::vars)

-- Specialized version of List to keep track of initialized variables
namespace Initialized
  -- public export
  -- data InitializedVariables : Type where
  --   Nil : InitializedVariables
  --   (::) : Nat -> InitializedVariables -> InitializedVariables

  -- public export
  -- data NameInitialized : Nat -> InitializedVariables -> Type where
  --   Here : NameInitialized name (name::init)
  --   There : NameInitialized name init ->
  --           NameInitialized name (newName::init)

    public export
    data NameInitialized : Nat -> Variables -> Type where
      Here : (prf : VariableDoesNotExist (MkVar name jty Init) vars) =>
             NameInitialized name ((MkVar name jty Init)::vars)
      There : (prf : VariableDoesNotExist var vars) =>
              NameInitialized name vars ->
              NameInitialized name (var::vars)

namespace Initialize
    public export
    data Initialize : Nat -> Variables -> Variables -> Type where
      Here : (prf : VariableDoesNotExist (MkVar name jty init) vars) =>
             Initialize name ((MkVar name jty init)::vars) ((MkVar name jty Init)::vars)
      There : (prf : VariableDoesNotExist var oldVars) =>
              (prf' : VariableDoesNotExist var newVars) =>
              Initialize name oldVars newVars ->
              Initialize name (var :: oldVars) (var :: newVars)

public export
length : Variables -> Nat
length [] = Z
length (_::vars) = S $ length vars

public export
getType : (vars : Variables) -> (k : Fin $ length vars) -> JType
getType ((MkVar _ jty _) :: vars) FZ = jty
getType (_ :: vars) (FS k) = getType vars k

public export
DecEq JType where
  decEq JBool JBool = Yes Refl
  decEq JBool JInt = No (\case Refl impossible)

  decEq JInt JInt = Yes Refl
  decEq JInt JBool = No (\case Refl impossible)

