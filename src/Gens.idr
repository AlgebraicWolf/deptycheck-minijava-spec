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
genInt _ = elements [-5..5]

genFin : Fuel -> (n : Nat) -> Gen $ Fin n
genFin _ Z = empty
genFin _ (S n) = (elements . forget) $ allFins n

variablesToList : Variables -> List Variable
variablesToList [] = []
variablesToList (var :: vars) = var :: variablesToList vars

variablesToGen : Variables -> Gen Variable
variablesToGen = elements . variablesToList

listExistsOfTypeVars : (vars : Variables) -> List (nm : Nat ** jty : JType ** ExistsOfType nm jty vars)
listExistsOfTypeVars [] = []
listExistsOfTypeVars ((::) @{NameAvailable prf} (MkVar nm jty) vars') = (nm ** jty ** THere) :: ((\(nm ** jty ** p) => (nm ** jty ** TThere p)) <$> (listExistsOfTypeVars vars'))

listExistsOfTypeJTypeVars : (jty : JType) -> (vars : Variables) -> List (nm : Nat ** ExistsOfType nm jty vars)
listExistsOfTypeJTypeVars jty vars = catMaybes $ (filterByJType <$> listExistsOfTypeVars vars) where
  filterByJType : (nm : Nat ** jty' : JType ** ExistsOfType nm jty' vars) -> Maybe (nm : Nat ** ExistsOfType nm jty vars)
  filterByJType (nm ** jty' ** p) = case jty `decEq` jty' of
                                            (Yes prf) => rewrite prf in Just (nm ** p)
                                            (No contra) => Nothing

listExistsOfTypeNameVars : (nm : Nat) -> (vars : Variables) -> List (jty : JType ** ExistsOfType nm jty vars)
listExistsOfTypeNameVars nm vars = catMaybes $ filterByName <$> listExistsOfTypeVars vars where
  filterByName : (nm' : Nat ** jty : JType ** ExistsOfType nm' jty vars) -> Maybe (jty : JType ** ExistsOfType nm jty vars)
  filterByName (nm' ** snd) = case nm `decEq` nm' of
                                          (Yes prf) => rewrite prf in Just snd
                                          (No contra) => Nothing

listExistsOfTypeNameJTypeVars : (nm : Nat) -> (jty : JType) -> (vars : Variables) -> List $ ExistsOfType nm jty vars
listExistsOfTypeNameJTypeVars nm jty vars = catMaybes $ filterByName <$> listExistsOfTypeJTypeVars jty vars where
  filterByName : (nm' : Nat ** ExistsOfType nm' jty vars) -> Maybe $ ExistsOfType nm jty vars
  filterByName (nm' ** p) = case nm `decEq` nm' of
                                   (Yes prf) => rewrite prf in Just p
                                   (No contra) => Nothing

genExistsOfTypeVars : Fuel -> (vars : Variables) -> Gen (nm : Nat ** jty : JType ** ExistsOfType nm jty vars)
genExistsOfTypeVars _ vars = elements $ listExistsOfTypeVars vars

genExistsOfTypeJTypeVars : Fuel -> (jty : JType) -> (vars : Variables) -> Gen (nm : Nat ** ExistsOfType nm jty vars)
genExistsOfTypeJTypeVars _ jty vars = elements $ listExistsOfTypeJTypeVars jty vars

genExistsOfTypeNameVars : Fuel -> (nm : Nat) -> (vars : Variables) -> Gen (jty : JType ** ExistsOfType nm jty vars)
genExistsOfTypeNameVars _ nm vars = elements $ listExistsOfTypeNameVars nm vars

genExistsOfTypeNameJTypeVars : Fuel -> (nm : Nat) -> (jty : JType) -> (vars : Variables) -> Gen $ ExistsOfType nm jty vars
genExistsOfTypeNameJTypeVars _ nm jty vars = elements $ listExistsOfTypeNameJTypeVars nm jty vars

genExpressionVarsInit' : Fuel ->
                         (vars : Variables) ->
                         (init : InitializedVariables) ->
                         Gen (res : JType ** Expression vars init res)
genExpressionVarsInit' f vars init = genExpressionVarsInit @{genInt} @{genJType} @{genExistsOfTypeVars} f vars init

genExpression' : Fuel ->
                 (vars : Variables) ->
                 (init : InitializedVariables) ->
                 (res : JType) ->
                 Gen $ Expression vars init res
genExpression' f vars init res = genExpression @{genInt} @{genJType} @{genExistsOfTypeNameJTypeVars} @{genExistsOfTypeJTypeVars} f vars init res

genStatementAllGiven' : Fuel ->
                        (vars : Variables) ->
                        (init : InitializedVariables) ->
                        Gen $ Statement vars init
genStatementAllGiven' f vars init = genStatementAllGiven @{genJType} @{genInt} @{genExistsOfTypeVars} @{genExistsOfTypeNameVars} @{genExistsOfTypeNameJTypeVars} @{genExpression'} @{genExpressionVarsInit'} f vars init

genExpressionVars' : Fuel -> (vars : Variables) -> Gen (init : InitializedVariables ** res : JType ** Expression vars init res)
genExpressionVars' f vars = genExpressionVars  @{genInt} @{genJType} @{genExistsOfTypeVars} @{genExistsOfTypeNameVars} @{genExistsOfTypeJTypeVars} @{genExistsOfTypeNameJTypeVars} f vars

genStatementVars' : Fuel -> (vars : Variables) -> Gen (init : InitializedVariables ** Statement vars init)
genStatementVars' f vars = genStatementVars @{genJType} @{genInt} @{genExistsOfTypeVars} @{genExistsOfTypeNameVars} @{genExistsOfTypeJTypeVars} @{genExistsOfTypeNameJTypeVars} @{genExpression'} @{genExpressionVarsInit'} @{genExpressionVars'} @{genStatementAllGiven'} f vars

genMainClass : Fuel -> Gen $ MainClass
genMainClass fuel = (\(_ ** stmt) => MkMain "MainClass" stmt) <$> (genStatementVars' fuel [MkVar 1 JBool, MkVar 0 JBool])

export
genProgram : Fuel -> Gen $ Program
genProgram fuel = MkProgram <$> genMainClass fuel

