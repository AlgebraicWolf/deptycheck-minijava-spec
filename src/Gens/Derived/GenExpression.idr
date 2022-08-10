module Gens.Derived.GenExpression

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Auto

import public Spec.Class

%default total
%language ElabReflection

%hint
UsedConstructorDerivator : ConstructorDerivator
UsedConstructorDerivator = LeastEffort

%logging "deptycheck.derive" 5

export
genExpression : Fuel ->
                (Fuel -> Gen Int) =>
                (Fuel -> Gen JType) =>
                (Fuel -> (name : Nat) -> (jty : JType) -> (vars : Variables) -> Gen $ ExistsOfType name jty vars) =>
                (Fuel -> (jty : JType) -> (vars : Variables) -> Gen (name : Nat ** ExistsOfType name jty vars)) =>
                (vars : Variables)  ->
                (init : InitializedVariables) ->
                (res : JType) ->
                Gen $ Expression vars init res
genExpression = deriveGen
