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
genExpressionVars : Fuel ->
                (Fuel -> Gen Int) =>
                (Fuel -> Gen JType) =>
                (Fuel -> (n : Nat) -> Gen $ Fin n) =>
                (res : JType) ->
                Gen $ (vars : Variables ** Expression vars res)
genExpressionVars = deriveGen
