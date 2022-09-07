module Gens.Derived.GenVariableDoesNotExist

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Auto
import Decidable.Equality
import Spec.Aspects.Variables

%default total
%language ElabReflection

%hint
UsedConstructorDerivator : ConstructorDerivator
UsedConstructorDerivator = LeastEffort

%logging "deptycheck.derive" 5

export
genVariableDoesNotExistVar : Fuel ->
                             (var : Variable) ->
                             Gen (vars : Variables ** VariableDoesNotExist var vars)
genVariableDoesNotExistVar = deriveGen

export
genVariableDoesNotExistVars : Fuel ->
                              (vars : Variables) ->
                              Gen (var : Variable ** VariableDoesNotExist var vars)
genVariableDoesNotExistVars = deriveGen

export
genVariableDoesNotExist : Fuel ->
                          Gen (var : Variable ** vars : Variables ** VariableDoesNotExist var vars)
genVariableDoesNotExist = deriveGen

export
genVariableDoesNotExistAll : Fuel ->
                             (var : Variable) ->
                             (vars : Variables) ->
                             Gen $ VariableDoesNotExist var vars
genVariableDoesNotExistAll = deriveGen
