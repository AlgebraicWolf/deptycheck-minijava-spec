module Gens.Manual.GenNatNotEqual

import Data.Fuel
import Spec.Variables
import Test.DepTyCheck.Gen
import Decidable.Equality

import Gens.Manual.GenNat

%default total

prevNats : Nat -> List Nat
prevNats 0 = []
prevNats (S n) = [0..n]

nextNats : Fuel -> Nat -> List Nat
nextNats fl n = [S n .. S n + flToNat fl]

export
genNatNotEqualAll : Fuel ->
                    (n : Nat) ->
                    (m : Nat) ->
                    Gen $ NatNotEqual n m
genNatNotEqualAll _ n m = case decEq n m of
                               (Yes _) => empty
                               (No contra) => pure $ NotRefl contra

natNotEqualSym : NatNotEqual n m -> NatNotEqual m n
natNotEqualSym (NotRefl f) = NotRefl $ (\prf => f $ sym prf)

export
genNatNotEqual0 : Fuel ->
                  (n : Nat) ->
                  Gen (m : Nat ** NatNotEqual n m)
genNatNotEqual0 fl n = do
  m <- elements $ prevNats n ++ nextNats fl n
  case decEq n m of
    (Yes prf) => empty -- Shouldn't actually be triggered
    (No contra) => pure $ (m ** NotRefl contra)

export
genNatNotEqual1 : Fuel ->
                  (n : Nat) ->
                  Gen (m : Nat ** NatNotEqual m n)
genNatNotEqual1 fl m = do
  (n ** prf) <- genNatNotEqual0 fl m
  pure (n ** natNotEqualSym prf)

export
genNatNotEqual : Fuel ->
                 Gen (n : Nat ** m : Nat ** NatNotEqual n m)
genNatNotEqual fl = do
  n <- genNat fl
  (m ** prf) <- genNatNotEqual0 fl n
  pure (n ** m ** prf)

