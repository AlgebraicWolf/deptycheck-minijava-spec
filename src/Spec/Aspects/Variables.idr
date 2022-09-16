module Spec.Aspects.Variables

import Decidable.Equality
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
DecEq InitState where
  decEq NotInit NotInit = Yes Refl
  decEq Init Init = Yes Refl
  decEq NotInit Init = No $ \case Refl impossible
  decEq Init NotInit = No $ \case Refl impossible

public export
data Variable : Type where
  MkVar : Nat -> JType -> InitState -> Variable

public export
DecEq Variable where
  decEq (MkVar nm jty init) (MkVar nm' jty' init') = case (decEq nm nm', decEq jty jty', decEq init init') of
                                                       (Yes Refl, Yes Refl, Yes Refl) => Yes Refl
                                                       (No contra, _, _) => No $ \case Refl => contra Refl
                                                       (_, No contra, _) => No $ \case Refl => contra Refl
                                                       (_, _, No contra) => No $ \case Refl => contra Refl

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

natNotEqualUniqueness : (prf : NatNotEqual n m) -> (prf' : NatNotEqual n m) -> prf = prf'
natNotEqualUniqueness SZ SZ = Refl
natNotEqualUniqueness ZS ZS = Refl
natNotEqualUniqueness (NotEqImpSNotEq prf) (NotEqImpSNotEq prf') = cong NotEqImpSNotEq $ natNotEqualUniqueness prf prf'

nameDifferentUniqueness : (prf : NameDifferent name var) -> (prf' : NameDifferent name var) -> prf = prf'
nameDifferentUniqueness (DiffName prf) (DiffName prf') = cong DiffName $ natNotEqualUniqueness prf prf'

nameAvailabilityUniqueness : (prf : NameDoesNotExist name vars) -> (prf' : NameDoesNotExist name vars) -> prf = prf'
nameAvailabilityUniqueness NoVars NoVars = Refl
nameAvailabilityUniqueness (DeclDiff recursive_unique  new_unique)
                           (DeclDiff recursive_unique' new_unique')
                           with
                           ( nameAvailabilityUniqueness recursive_unique recursive_unique'
                           , nameDifferentUniqueness new_unique new_unique')
  nameAvailabilityUniqueness (DeclDiff recursive_unique new_unique)
                             (DeclDiff recursive_unique new_unique)
                             | (Refl, Refl) = Refl

-- Uniqueness of nonexistence proofs
variableNonexistenceUnique : (prf : VariableDoesNotExist var vars) -> (prf' : VariableDoesNotExist var vars) -> prf = prf'
variableNonexistenceUnique (NameAvailable prf) (NameAvailable prf') = cong NameAvailable $ nameAvailabilityUniqueness prf prf'

public export
DecEq Variables where
  decEq [] [] = Yes Refl
  decEq [] (var :: vars) = No $ \case Refl impossible
  decEq (var :: vars) [] = No $ \case Refl impossible
  decEq ((::) @{prf} var vars) ((::) @{prf'} var' vars') with (decEq var var', decEq vars vars')
    decEq ((::) @{prf} var vars) ((::) @{prf'} var vars) | (Yes Refl, Yes Refl) with (variableNonexistenceUnique prf prf')
      decEq((::) @{prf} var vars) ((::) @{prf} var vars) | (Yes Refl, Yes Refl) | Refl = Yes Refl
    decEq (var :: vars) (var' :: vars') | (No contra, _) = No $ \case Refl => contra Refl
    decEq (var :: vars) (var' :: vars') | (_, No contra) = No $ \case Refl => contra Refl

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
      Here : (prf  : VariableDoesNotExist (MkVar name jty init) vars) =>
             (prf' : VariableDoesNotExist (MkVar name jty Init) vars) =>
             Initialize name ((MkVar name jty init)::vars) ((MkVar name jty Init)::vars)
      There : (prf  : VariableDoesNotExist var oldVars) =>
              (prf' : VariableDoesNotExist var newVars) =>
              Initialize name oldVars newVars ->
              Initialize name (var :: oldVars) (var :: newVars)

-- Proof that one list of variables is formed by extending another list of variables
-- While proving, construct a new variable list that will be used outside the block
namespace PrefixOf
  public export
  data PrefixOf : Variables -> Variables -> Variables -> Type where
    -- Empty ctx
    Empty : PrefixOf [] [] []
    -- Initialize inside the block
    InitInside : (prf   : NameDoesNotExist nm vars) =>
                 (prf'  : NameDoesNotExist nm varsInside) =>
                 (prf'' : NameDoesNotExist nm varsOutside) =>
                 PrefixOf vars varsInside varsOutside ->
                 PrefixOf ((MkVar nm jty NotInit)::vars) ((MkVar nm jty Init)::varsInside) ((MkVar nm jty Init)::varsOutside)
    -- Don't change
    DontChange : (prf   : VariableDoesNotExist var vars) =>
                 (prf'  : VariableDoesNotExist var varsInside) =>
                 (prf'' : VariableDoesNotExist var varsOutside) =>
                 PrefixOf vars varsInside varsOutside ->
                 PrefixOf (var::vars) (var::varsInside) (var::varsOutside)
    -- Declare variable in the inner scope
    DeclNew : (prf : VariableDoesNotExist var varsInside) =>
               PrefixOf vars varsInside varsOutside ->
               PrefixOf vars (var :: varsInside) varsOutside

public export
length : Variables -> Nat
length [] = Z
length (_::vars) = S $ length vars

public export
getType : (vars : Variables) -> (k : Fin $ length vars) -> JType
getType ((MkVar _ jty _) :: vars) FZ = jty
getType (_ :: vars) (FS k) = getType vars k

