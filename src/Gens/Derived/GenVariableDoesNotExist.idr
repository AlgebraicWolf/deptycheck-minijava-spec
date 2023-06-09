module Gens.Derived.GenVariableDoesNotExist

import Test.DepTyCheck.Gen
import Deriving.DepTyCheck.Gen
import Decidable.Equality
import Spec.Variables

%default total
%language ElabReflection

%hint
UsedConstructorDerivator : ConstructorDerivator
UsedConstructorDerivator = LeastEffort

%logging "deptycheck.derive" 5

export
genVariableDoesNotExistVar : Fuel ->
                             (Fuel -> Gen Nat) =>
                             (Fuel -> (nm : Nat) -> Gen (vars : Variables ** NameDoesNotExist nm vars)) =>
                             (var : Variable) ->
                             Gen (vars : Variables ** VariableDoesNotExist var vars)
genVariableDoesNotExistVar = deriveGen

export
genVariableDoesNotExistVars : Fuel ->
                              (Fuel -> Gen Nat) =>
                              (Fuel -> (vars : Variables) -> Gen (nm : Nat ** NameDoesNotExist nm vars)) =>
                              (vars : Variables) ->
                              Gen (var : Variable ** VariableDoesNotExist var vars)
genVariableDoesNotExistVars = deriveGen

export
genVariableDoesNotExist : Fuel ->
                          (Fuel -> Gen Nat) =>
                          (Fuel -> Gen (nm : Nat ** vars : Variables ** NameDoesNotExist nm vars)) =>
                          Gen (var : Variable ** vars : Variables ** VariableDoesNotExist var vars)
genVariableDoesNotExist = deriveGen

export
genVariableDoesNotExistAll : Fuel ->
                             (Fuel -> Gen Nat) =>
                             (Fuel -> (nm : Nat) -> (vars : Variables) -> Gen $ NameDoesNotExist nm vars) =>
                             (var : Variable) ->
                             (vars : Variables) ->
                             Gen $ VariableDoesNotExist var vars
genVariableDoesNotExistAll = deriveGen
