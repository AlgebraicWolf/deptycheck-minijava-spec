module Gens.Derived.GenNameDoesNotExist

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Auto
import Decidable.Equality
import Spec.Variables

%default total
%language ElabReflection

%hint
UsedConstructorDerivator : ConstructorDerivator
UsedConstructorDerivator = LeastEffort

%logging "deptycheck.derive" 5

export
genNameDoesNotExistVar : Fuel ->
                         (nm : Nat) ->
                         Gen (vars : Variables ** NameDoesNotExist nm vars)
genNameDoesNotExistVar = deriveGen

export
genNameDoesNotExistVars : Fuel ->
                         (vars : Variables) ->
                         Gen (nm : Nat ** NameDoesNotExist nm vars)
genNameDoesNotExistVars = deriveGen

export
genNameDoesNotExist : Fuel ->
                      Gen (nm : Nat ** vars : Variables ** NameDoesNotExist nm vars)
genNameDoesNotExist = deriveGen

export
genNameDoesNotExistAll : Fuel ->
                         (nm : Nat) ->
                         (vars : Variables) ->
                         Gen $ NameDoesNotExist nm vars
genNameDoesNotExistAll = deriveGen
