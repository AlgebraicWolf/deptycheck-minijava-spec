module Gens.Derived.GenStatement

import Test.DepTyCheck.Gen
import Deriving.DepTyCheck.Gen

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
               -- Generators for `ExistsOfType`
               (Fuel -> (vars : Variables) -> Gen (name : Nat ** jty : JType ** ExistsOfType name jty vars)) =>
               (Fuel -> (name : Nat) -> (vars : Variables) -> Gen (jty : JType ** ExistsOfType name jty vars)) =>
               (Fuel -> (jty : JType) -> (vars : Variables) -> Gen (name : Nat ** ExistsOfType name jty vars)) =>
               (Fuel -> (name : Nat) -> (jty : JType) -> (vars : Variables) -> Gen $ ExistsOfType name jty vars) =>
               (Fuel -> Gen (name : Nat ** jty : JType ** vars : Variables ** ExistsOfType name jty vars)) =>
               -- Generators for `Expression`
               (Fuel -> (vars : Variables) -> (result : JType) -> Gen $ Expression vars result) =>
               (Fuel -> (vars : Variables) -> Gen (result : JType ** Expression vars result)) =>
               (Fuel -> Gen (vars : Variables ** result : JType ** Expression vars result)) =>
               -- Generators for `VariableDoesNotExist`
               (Fuel -> (var : Variable) -> Gen (vars : Variables ** VariableDoesNotExist var vars)) =>
               (Fuel -> Gen (var : Variable ** vars : Variables ** VariableDoesNotExist var vars)) =>
               -- Generators for `Initialize`
               -- (Fuel -> (name : Nat) -> (oldVars : Variables) -> Gen (newVars : Variables ** Initialize name oldVars newVars)) =>
               (Fuel -> (oldVars : Variables) -> (newVars : Variables) -> Gen (name : Nat ** Initialize name oldVars newVars)) =>
               (Fuel -> (name : Nat) -> (oldVars : Variables) -> (newVars : Variables) -> Gen $ Initialize name oldVars newVars) =>
               (Fuel -> (newVars : Variables) -> Gen (name : Nat ** oldVars : Variables ** Initialize name oldVars newVars)) =>
               Gen $ (preV : Variables ** postV : Variables ** Statement preV postV)
-- genStatement = deriveGen
