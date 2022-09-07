module Gens.Derived.GenNameDoesNotExist

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

genNameDoesNotExistAll : Fuel ->
                         (nm : Nat) ->
                         (vars : Variables) ->
                         Gen $ NameDoesNotExist nm vars
-- genNameDoesNotExistAll = deriveGen

genNameDoesNotExistNm : Fuel ->
                        -- Generators for primitive types
                        (Fuel -> Gen JType) =>
                        (Fuel -> Gen InitState) =>
                        (Fuel -> Gen Nat) =>
                        (Fuel -> (nm : Nat) -> (vars : Variables) -> Gen $ NameDoesNotExist nm vars) =>
                        (nm : Nat) ->
                        Gen (vars : Variables ** NameDoesNotExist nm vars)
genNameDoesNotExistNm = deriveGen
