module Gens

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Auto

import public Spec.Class

%default total
%language ElabReflection

genJType : Fuel -> Gen JType
genJType = deriveGen @{MainCoreDerivator @{LeastEffort}}

genInt : Fuel -> Gen Int
genInt _ = elements [-100..100]

-- Generate expression of desired signature
genExpression : Fuel -> (Fuel -> Gen Int) => (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpression = deriveGen @{MainCoreDerivator @{LeastEffort}}
