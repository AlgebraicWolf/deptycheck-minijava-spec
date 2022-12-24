module Gens.Manual.GenNat

import Data.Fuel
import Test.DepTyCheck.Gen

%default total

export
flToNat : Fuel -> Nat
flToNat = flToNat' 0 where
  flToNat' : Nat -> Fuel -> Nat
  flToNat' acc Dry = acc
  flToNat' acc (More fl) = flToNat' (S acc) fl

export
genNat : Fuel -> Gen Nat
-- genNat fl = elements [0..flToNat fl]
genNat _ = elements [0..3]
