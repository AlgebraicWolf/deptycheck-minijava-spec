module Gens.Derived.GenStatement

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
genStatement : Fuel ->
               (Fuel -> Gen JType) =>
               (Fuel -> Gen Int) =>
               (Fuel -> (n : Nat) -> Gen $ Fin n) =>
               (Fuel -> (vars : Variables) -> (res : JType) -> Gen $ Expression vars res) =>
               (Fuel -> (vars : Variables) -> Gen $ Statement vars) =>
               (Fuel -> (res : JType) -> Gen (vars : Variables ** Expression vars res)) =>
               Gen $ (vars : Variables ** Statement vars)
genStatement = ?genStatement_rhs
