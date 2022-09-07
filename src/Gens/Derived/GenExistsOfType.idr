module Gens.Derived.GenExistsOfType

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
genExistsOfTypeVars : Fuel ->
                      (vars : Variables) ->
                      Gen (nm : Nat ** jty : JType ** ExistsOfType nm jty vars)

genExistsOfTypeVars = deriveGen

export
genExistsOfTypeNameVars : Fuel ->
                          (nm : Nat) ->
                          (vars : Variables) ->
                          Gen (jty : JType ** ExistsOfType nm jty vars)
genExistsOfTypeNameVars = deriveGen

export
genExistsOfTypeJTyVars : Fuel ->
                         (jty : JType) ->
                         (vars : Variables) ->
                         Gen (nm : Nat ** ExistsOfType nm jty vars)
genExistsOfTypeJTyVars = deriveGen

export
genExistsOfTypeAll : Fuel ->
                     (nm : Nat) ->
                     (jty : JType) ->
                     (vars : Variables) ->
                     Gen $ ExistsOfType nm jty vars
genExistsOfTypeAll = deriveGen

export
genExistsOfType : Fuel ->
                  (Fuel -> (var : Variable) -> Gen (vars : Variables ** VariableDoesNotExist var vars)) =>
                  (Fuel -> (vars : Variables) -> Gen (nm : Nat ** jty : JType ** ExistsOfType nm jty vars)) =>
                  Gen (nm : Nat ** jty : JType ** vars : Variables ** ExistsOfType nm jty vars)
genExistsOfType = deriveGen
