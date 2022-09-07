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
genExpressionAll : Fuel ->
                (Fuel -> Gen Int) =>
                (Fuel -> Gen JType) =>
                (Fuel -> (name : Nat) -> (jty : JType) -> (vars : Variables) -> Gen $ ExistsOfType name jty vars) =>
                (Fuel -> (jty : JType) -> (vars : Variables) -> Gen (name : Nat ** ExistsOfType name jty vars)) =>
                (vars : Variables)  ->
                (res : JType) ->
                Gen $ Expression vars res
genExpressionAll = deriveGen

export
genExpressionVars : Fuel ->
                    (Fuel -> Gen Int) =>
                    (Fuel -> Gen JType) =>
                    (vars : Variables) ->
                    Gen (res : JType ** Expression vars res)
genExpressionVars = deriveGen

export
genExpression : Fuel ->
                (Fuel -> Gen Int) =>
                (Fuel -> Gen JType) =>
                (Fuel -> (var : Variable) -> Gen (vars : Variables ** VariableDoesNotExist var vars)) =>
                (Fuel -> (vars : Variables) -> Gen (var : Variable ** VariableDoesNotExist var vars)) =>
                (Fuel -> Gen (var : Variable ** vars : Variables ** VariableDoesNotExist var vars)) =>
		Gen (vars : Variables ** res : JType ** Expression vars res)
genExpression = deriveGen
