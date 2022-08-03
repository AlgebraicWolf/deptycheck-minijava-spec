module Gens.Derived.GenExpressionVars

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
genExpressionVarsInit : Fuel ->
                (Fuel -> Gen Int) =>
                (Fuel -> Gen JType) =>
                (Fuel -> (vars : Variables) -> Gen (name : Nat ** jty : JType ** ExistsOfType name jty vars)) =>
                (vars : Variables) ->
                (init : InitializedVariables) ->
                Gen (res : JType ** Expression vars init res)
genExpressionVarsInit = deriveGen
