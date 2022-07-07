module Gens

import Data.Fin
import Data.List.Lazier
import Decidable.Equality.Core

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
-- genExpression : Fuel -> (Fuel -> Gen Int) => (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
-- genExpression = deriveGen @{MainCoreDerivator @{LeastEffort}}

-- Generator of expressions with desired signature
-- We can generate either a literal or a read from a variable

genExpressionVars : Fuel -> (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpressionVars _ 0 [] type = uniform []
genExpressionVars _ (S n) (x::_) type = uniform $ do
                                                    k <- (fromList $ forget $ allFins n)
                                                    case decEq (getType k (x::_)) type of
                                                      (Yes prf) => pure $ FromIdentifier @{eqToProof k (x::_) type prf} k
                                                      (No contra) => []

genExpressionLiteral : Fuel -> (Fuel -> Gen Int) => (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpressionLiteral fuel n vars JBool = uniform [BoolTrue, BoolFalse]
genExpressionLiteral @{genInt} fuel n vars JInt = (\x => IntegerLiteral x) <$> genInt fuel

genExpression : Fuel -> (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpression fuel n vars res = genExpressionLiteral @{genInt} fuel n vars res <|> genExpressionVars fuel n vars res

-- Generator of statements with desired starting point and free endpoint
-- genStatement : Fuel -> (Fuel -> Gen Int) => Gen $ (n : Nat ** postV : Variables n ** Statement n postV)
-- genStatement = deriveGen @{MainCoreDerivator @{LeastEffort}}
genStatement : Fuel -> Gen $ (n : Nat ** postV : Variables n ** Statement n postV)
genStatement Dry = do pure (0 ** [] ** Empty)
genStatement (More x) = let prev = genStatement x in do
                          (n ** vars ** stmt) <- prev
                          pure (_ ** _ ** VarDeclaration JBool stmt)



