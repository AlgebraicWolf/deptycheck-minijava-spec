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

genInt : Gen Int
genInt = elements [-100..100]

-- Generate expression of desired signature
-- genExpression : Fuel -> (Fuel -> Gen Int) => (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
-- genExpression = deriveGen @{MainCoreDerivator @{LeastEffort}}

-- Generator of expressions with desired signature
-- We can generate either a literal or a read from a variable

genExpressionVars : (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpressionVars 0 [] type = empty
genExpressionVars (S n) (x::_) type = elements $ do
                                                    k <- forget $ allFins n
                                                    case decEq (getType k (x::_)) type of
                                                      (Yes prf) => pure $ FromIdentifier @{eqToProof k (x::_) type prf} k
                                                      (No contra) => []

genExpressionLiteral : Gen Int => (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpressionLiteral n vars JBool = elements [BoolTrue, BoolFalse]
genExpressionLiteral @{genInt} n vars JInt = (\x => IntegerLiteral x) <$> genInt

genExpression : (n : Nat) -> (vars : Variables n) -> (res : JType) -> Gen $ Expression n vars res
genExpression n vars res = genExpressionLiteral @{genInt} n vars res <|> genExpressionVars n vars res

-- Generator of statements with desired starting point and free endpoint
-- genStatement : Fuel -> (Fuel -> Gen Int) => Gen $ (n : Nat ** postV : Variables n ** Statement n postV)
-- genStatement = deriveGen @{MainCoreDerivator @{LeastEffort}}
genStatement : Fuel -> Gen $ (n : Nat ** postV : Variables n ** Statement n postV)
genStatement Dry = pure (_ ** _ ** Empty)
genStatement (More x) = let prev = genStatement x in
                        let var_decl = do
                          (n ** vars ** stmt) <- prev
                          elements [(_ ** _ ** VarDeclaration JBool stmt), (_ ** _ ** VarDeclaration JInt stmt)] in
                        let assignment = do
                          (n ** vars ** stmt) <- prev
                          case n of
                            Z => empty
                            (S m) => oneOf $ do
                                      k <- (forget $ allFins m)
                                      pure $ genExpression n vars (getType k vars) >>= (\expr => case choose (isValidExpr expr stmt) of
                                                                                                 Left prf => pure (_ ** _ ** Assignment k expr stmt prf)
                                                                                                 Right _ => empty)
                          in
                          var_decl <|> assignment

genMainClass : Fuel -> Gen $ MainClass
genMainClass fuel = (\(_ ** _ ** stmt) => MkMain "MainClass" stmt) <$> genStatement fuel

export
genProgram : Fuel -> Gen $ Program
genProgram fuel = MkProgram <$> genMainClass fuel

