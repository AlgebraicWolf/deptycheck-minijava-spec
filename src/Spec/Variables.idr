module Spec.Variables

import Decidable.Equality
import Data.Fin

import public Spec.Types

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

-- public export
-- data NatNotEqual : Nat -> Nat -> Type where
--   SZ : NatNotEqual (S n) Z
--   ZS : NatNotEqual Z (S n)
--   NotEqImpSNotEq : NatNotEqual n m ->
--                    NatNotEqual (S n) (S m)

public export
data NatNotEqual : Nat -> Nat -> Type where
  NotRefl : Not (x = y) -> NatNotEqual x y

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



func_ext : (f : a -> b) -> (g : a -> b) -> ((x : a) -> f x = g x) -> f = g
func_ext f g pointwise = believe_me $ Refl {x=Z}

voidUnique : (x : Void) -> (y : Void) -> x = y
voidUnique x y impossible

natNotEqualUniqueness : (prf : NatNotEqual n m) -> (prf' : NatNotEqual n m) -> prf = prf'
-- natNotEqualUniqueness SZ SZ = Refl
-- natNotEqualUniqueness ZS ZS = Refl
-- natNotEqualUniqueness (NotEqImpSNotEq prf) (NotEqImpSNotEq prf') = cong NotEqImpSNotEq $ natNotEqualUniqueness prf prf'
natNotEqualUniqueness (NotRefl f) (NotRefl g) = cong NotRefl $ func_ext f g (\x => voidUnique (f x) (g x))

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
DecEq (NameDoesNotExist nm vars) where
  decEq x y = Yes $ nameAvailabilityUniqueness x y

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

public export
length : Variables -> Nat
length [] = Z
length (_::vars) = S $ length vars

public export
getType : (vars : Variables) -> (k : Fin $ length vars) -> JType
getType ((MkVar _ jty _) :: vars) FZ = jty
getType (_ :: vars) (FS k) = getType vars k

