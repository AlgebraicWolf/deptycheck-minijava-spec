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
                (Fuel -> (n : Nat) -> Gen $ Fin n) =>
                (Fuel -> (res : JType) -> Gen $ (vars : Variables ** Expression vars res)) =>
                (vars : Variables)  ->
                (res : JType) ->
                Gen $ Expression vars res
genExpression = deriveGen
-- genExpression = ?genExpression_rhs
