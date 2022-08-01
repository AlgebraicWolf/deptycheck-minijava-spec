module Gens

import Data.Fin
import Data.List.Lazier
import Decidable.Equality.Core

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Auto

import public Spec.Class

import Gens.Derived

%default total
%language ElabReflection

%hint
UsedConstructorDerivator : ConstructorDerivator
UsedConstructorDerivator = LeastEffort

%logging "deptycheck.derive" 5

genJType : Fuel -> Gen JType
genJType _ = elements [JBool, JInt]

genInt : Fuel -> Gen Int
genInt _ = elements [-100..100]

genFin : Fuel -> (n : Nat) -> Gen $ Fin n
genFin _ Z = empty
genFin _ (S n) = (elements . forget) $ allFins n

genExpressionVars' : Fuel -> (res : JType) -> Gen $ (vars : Variables ** Expression vars res)
genExpressionVars' f res = genExpressionVars @{genInt} @{genJType} @{genFin} f res

genExpression' : Fuel -> (vars : Variables) -> (res : JType) -> Gen $ Expression vars res
genExpression' f vars res = genExpression @{genInt} @{genJType} @{genFin} @{genExpressionVars'} f vars res

genStatement : Fuel -> Gen $ (vars : Variables ** Statement vars)
genStatement Dry = pure (_ ** Empty)
genStatement f@(More x) = let prev = genStatement x in
                          let var_decl = do
                            (vars ** stmt) <- prev
                            elements [(_ ** VarDeclaration JBool stmt), (_ ** VarDeclaration JInt stmt)] in
                          let assignment = do
                            (vars ** stmt) <- prev
                            case vars of
                                 [] => empty
                                 (v :: vars') => oneOf $ do
                                   k <- forget $ allFins (length vars')
                                   pure $ genExpression' f vars (getType vars k) >>= (\expr => pure (_ ** Assignment vars k expr stmt))
                          in
                          assignment <|> var_decl

genMainClass : Fuel -> Gen $ MainClass
genMainClass fuel = (\(_ ** stmt) => MkMain "MainClass" stmt) <$> genStatement fuel

export
genProgram : Fuel -> Gen $ Program
genProgram fuel = MkProgram <$> genMainClass fuel

