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
genInt _ = elements [0]

genFin : Fuel -> (n : Nat) -> Gen $ Fin n
genFin _ Z = empty
genFin _ (S n) = (elements . forget) $ allFins n

variablesToList : Variables -> List Variable
variablesToList [] = []
variablesToList (var :: vars) = var :: variablesToList vars

variablesToGen : Variables -> Gen Variable
variablesToGen = elements . variablesToList

listExistsOfTypeVars : (vars : Variables) -> List (name : Nat ** jty : JType ** ExistsOfType name jty vars)
listExistsOfTypeVars [] = []
listExistsOfTypeVars ((::) @{NameAvailable prf} (MkVar name jty) vars') = (name ** jty ** THere) :: ((\(name ** jty ** p) => (name ** jty ** TThere p)) <$> (listExistsOfTypeVars vars'))

listExistsOfTypeJTypeVars : (jty : JType) -> (vars : Variables) -> List (name : Nat ** ExistsOfType name jty vars)
listExistsOfTypeJTypeVars jty vars = catMaybes $ (filterByJType <$> listExistsOfTypeVars vars) where
  filterByJType : (name : Nat ** jty' : JType ** ExistsOfType name jty' vars) -> Maybe (name : Nat ** ExistsOfType name jty vars)
  filterByJType (name ** jty' ** p) = case jty `decEq` jty' of
                                            (Yes prf) => rewrite prf in Just (name ** p)
                                            (No contra) => Nothing

listExistsOfTypeNameVars : (name : Nat) -> (vars : Variables) -> List (jty : JType ** ExistsOfType name jty vars)
listExistsOfTypeNameVars name vars = catMaybes $ filterByName <$> listExistsOfTypeVars vars where
  filterByName : (name' : Nat ** jty : JType ** ExistsOfType name' jty vars) -> Maybe (jty : JType ** ExistsOfType name jty vars)
  filterByName (name' ** snd) = case name `decEq` name' of
                                          (Yes prf) => rewrite prf in Just snd
                                          (No contra) => Nothing

listExistsOfTypeNameJTypeVars : (name : Nat) -> (jty : JType) -> (vars : Variables) -> List $ ExistsOfType name jty vars
listExistsOfTypeNameJTypeVars name jty vars = catMaybes $ filterByName <$> listExistsOfTypeJTypeVars jty vars where
  filterByName : (name' : Nat ** ExistsOfType name' jty vars) -> Maybe $ ExistsOfType name jty vars
  filterByName (name' ** p) = case name `decEq` name' of
                                   (Yes prf) => rewrite prf in Just p
                                   (No contra) => Nothing

genExistsOfTypeVars : Fuel -> (vars : Variables) -> Gen (name : Nat ** jty : JType ** ExistsOfType name jty vars)
genExistsOfTypeVars _ vars = elements $ listExistsOfTypeVars vars

genExistsOfTypeJTypeVars : Fuel -> (jty : JType) -> (vars : Variables) -> Gen (name : Nat ** ExistsOfType name jty vars)
genExistsOfTypeJTypeVars _ jty vars = elements $ listExistsOfTypeJTypeVars jty vars

genExistsOfTypeNameVars : Fuel -> (name : Nat) -> (vars : Variables) -> Gen (jty : JType ** ExistsOfType name jty vars)
genExistsOfTypeNameVars _ name vars = elements $ listExistsOfTypeNameVars name vars

genExistsOfTypeNameJTypeVars : Fuel -> (name : Nat) -> (jty : JType) -> (vars : Variables) -> Gen $ ExistsOfType name jty vars
genExistsOfTypeNameJTypeVars _ name jty vars = elements $ listExistsOfTypeNameJTypeVars name jty vars

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

