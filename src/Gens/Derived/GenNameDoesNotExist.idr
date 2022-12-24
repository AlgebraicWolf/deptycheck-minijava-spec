module Gens.Derived.GenNameDoesNotExist

import Test.DepTyCheck.Gen
import Deriving.DepTyCheck.Gen
import Decidable.Equality
import Spec.Variables

%default total
%language ElabReflection

%hint
UsedConstructorDerivator : ConstructorDerivator
UsedConstructorDerivator = LeastEffort

%logging "deptycheck.derive" 5

export
genNameDoesNotExistVar : Fuel ->
                         (Fuel -> Gen Nat) =>
                         (Fuel -> (n : Nat) -> (m : Nat) -> Gen $ NatNotEqual n m) =>
                         (Fuel -> (n : Nat) -> Gen (m : Nat ** NatNotEqual n m)) =>
                         (Fuel -> (m : Nat) -> Gen (n : Nat ** NatNotEqual n m)) =>
                         (Fuel -> Gen (n : Nat ** m : Nat ** NatNotEqual n m)) =>
                         (nm : Nat) ->
                         Gen (vars : Variables ** NameDoesNotExist nm vars)
genNameDoesNotExistVar = deriveGen

export
genNameDoesNotExistVars : Fuel ->
                          (Fuel -> Gen Nat) =>
                          (Fuel -> (n : Nat) -> (m : Nat) -> Gen $ NatNotEqual n m) =>
                          (Fuel -> (n : Nat) -> Gen (m : Nat ** NatNotEqual n m)) =>
                          (Fuel -> (m : Nat) -> Gen (n : Nat ** NatNotEqual n m)) =>
                          (Fuel -> Gen (n : Nat ** m : Nat ** NatNotEqual n m)) =>
                          (vars : Variables) ->
                          Gen (nm : Nat ** NameDoesNotExist nm vars)
genNameDoesNotExistVars = deriveGen

export
genNameDoesNotExist : Fuel ->
                      (Fuel -> Gen Nat) =>
                      (Fuel -> (n : Nat) -> (m : Nat) -> Gen $ NatNotEqual n m) =>
                      (Fuel -> (n : Nat) -> Gen (m : Nat ** NatNotEqual n m)) =>
                      (Fuel -> (m : Nat) -> Gen (n : Nat ** NatNotEqual n m)) =>
                      (Fuel -> Gen (n : Nat ** m : Nat ** NatNotEqual n m)) =>
                      Gen (nm : Nat ** vars : Variables ** NameDoesNotExist nm vars)
genNameDoesNotExist = deriveGen

export
genNameDoesNotExistAll : Fuel ->
                         (Fuel -> Gen Nat) =>
                         (Fuel -> (n : Nat) -> (m : Nat) -> Gen $ NatNotEqual n m) =>
                         (Fuel -> (n : Nat) -> Gen (m : Nat ** NatNotEqual n m)) =>
                         (Fuel -> (m : Nat) -> Gen (n : Nat ** NatNotEqual n m)) =>
                         (Fuel -> Gen (n : Nat ** m : Nat ** NatNotEqual n m)) =>
                         (nm : Nat) ->
                         (vars : Variables) ->
                         Gen $ NameDoesNotExist nm vars
genNameDoesNotExistAll = deriveGen
