module Gens.Derived.GenInitialize

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

export covering
printDerived : DerivatorCore => Type -> Elab Unit
printDerived ty = do
  ty <- quote ty
  logSugaredTerm "gen.auto.derive.infra" 0 "type" ty
  logMsg "gen.auto.derive.infra" 0 "\n\{show !(deriveGenExpr ty)}"

export
genInitialize : Fuel ->
                (Fuel -> (var : Variable) -> Gen (vars : Variables ** VariableDoesNotExist var vars)) =>
                (Fuel -> (var : Variable) -> (vars : Variables) -> Gen $ VariableDoesNotExist var vars) =>
                (Fuel -> (vars : Variables) -> Gen (var : Variable ** VariableDoesNotExist var vars)) =>
                Gen (nm : Nat ** oldVars : Variables ** newVars : Variables ** Initialize nm oldVars newVars)
genInitialize = deriveGen

export
genInitializeOld : Fuel ->
                   (Fuel -> (var : Variable) -> (vars : Variables) -> Gen $ VariableDoesNotExist var vars) =>
                   (oldVars : Variables) ->
                   Gen (nm : Nat ** newVars : Variables ** Initialize nm oldVars newVars)
genInitializeOld = deriveGen

export
genInitializeNew : Fuel ->
                   (Fuel -> (var : Variable) -> (vars : Variables) -> Gen $ VariableDoesNotExist var vars) =>
                   (newVars : Variables) ->
                   Gen (nm : Nat ** oldVars : Variables ** Initialize nm oldVars newVars)
genInitializeNew = deriveGen

export
genInitializeNameOld : Fuel ->
                       (Fuel -> (var : Variable) -> (vars : Variables) -> Gen $ VariableDoesNotExist var vars) =>
                       (Fuel -> (var : Variable) -> Gen (vars : Variables ** VariableDoesNotExist var vars)) =>
                       (Fuel -> (nm : Nat) -> (oldVars : Variables) -> (newVars : Variables) -> Gen $ Initialize nm oldVars newVars) =>
                       (nm : Nat) ->
                       (oldVars : Variables) ->
                       Gen (newVars : Variables ** Initialize nm oldVars newVars)
genInitializeNameOld = deriveGen

export
genInitializeOldNew : Fuel ->
                      (oldVars : Variables) ->
                      (newVars : Variables) ->
                      Gen (nm : Nat ** Initialize nm oldVars newVars)
genInitializeOldNew = deriveGen

export
genInitializeAll : Fuel ->
                   (nm : Nat) ->
                   (oldVars : Variables) ->
                   (newVars : Variables) ->
                   Gen $ Initialize nm oldVars newVars
genInitializeAll = deriveGen

