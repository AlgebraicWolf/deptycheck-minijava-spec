module Gens.Derived.GenStatementAllGiven

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
genStatementAllGiven : Fuel ->
               (Fuel -> Gen JType) =>
               (Fuel -> Gen Int) =>
               (Fuel -> Gen Nat) =>
               (Fuel -> (n : Nat) -> Gen $ Fin n) =>
               (Fuel -> (vars : Variables) -> (res : JType) -> Gen $ Expression vars res) =>
               (Fuel -> (res : JType) -> Gen (vars : Variables ** Expression vars res)) =>
               (vars : Variables) ->
               Gen $ Statement vars
genStatementAllGiven = ?genStatementAllGiven_rhs
