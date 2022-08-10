module Gens.Derived.GenBug

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

data NatNotEqual : Nat -> Nat -> Type where
  SuccZ : NatNotEqual (S n) Z
  ZSucc : NatNotEqual Z (S n)
  SuccSucc : NatNotEqual n m -> NatNotEqual (S n) (S m)

genNatNotEqual : Fuel ->
                 (n : Nat) ->
                 (m : Nat) ->
                 Gen $ NatNotEqual n m

genNatNotEqual = deriveGen
