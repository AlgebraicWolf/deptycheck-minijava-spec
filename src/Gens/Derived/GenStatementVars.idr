module Gens.Derived.GenStatementVars

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
genStatementVars : Fuel ->
               (Fuel -> Gen JType) =>
               (Fuel -> Gen Int) =>
               (Fuel -> (vars : Variables) -> Gen (name : Nat ** jty : JType ** ExistsOfType name jty vars)) =>
               (Fuel -> (name : Nat) -> (vars : Variables) -> Gen (jty : JType ** ExistsOfType name jty vars)) =>
               (Fuel -> (jty : JType) -> (vars : Variables) -> Gen (name : Nat ** ExistsOfType name jty vars)) =>
               (Fuel -> (name : Nat) -> (jty : JType) -> (vars : Variables) -> Gen $ ExistsOfType name jty vars) =>
               (Fuel -> (vars : Variables) -> (init : InitializedVariables) -> (result : JType) -> Gen $ Expression vars init result) =>
               (Fuel -> (vars : Variables) -> (init : InitializedVariables) -> Gen (result : JType ** Expression vars init result)) =>
               (Fuel -> (vars : Variables) -> Gen (init : InitializedVariables ** result : JType ** Expression vars init result)) =>
               (Fuel -> (vars : Variables) -> (init : InitializedVariables) -> Gen $ Statement vars init) =>
               (vars : Variables) ->
               Gen $ (init : InitializedVariables ** Statement vars init)
genStatementVars = deriveGen
