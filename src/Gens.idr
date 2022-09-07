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

-- Declarations of auxillary generators
genExistsOfType' : Fuel -> Gen (nm : Nat ** jty : JType ** vars : Variables ** ExistsOfType nm jty vars)


genExpressionAll' : Fuel -> (vars : Variables) -> (jty : JType) -> Gen $ Expression vars jty

genExpressionVars' : Fuel -> (vars : Variables) -> Gen (jty : JType ** Expression vars jty)

genExpression' : Fuel -> Gen (vars : Variables ** jty : JType ** Expression vars jty)


genInitialize' : Fuel -> Gen (nm : Nat ** oldVars : Variables ** newVars : Variables ** Initialize nm oldVars newVars)

genInitializeNameOld' : Fuel -> (nm : Nat) -> (oldVars : Variables) -> Gen (newVars : Variables ** Initialize nm oldVars newVars)

genInitializeNew' : Fuel -> (newVars : Variables) -> Gen (nm : Nat ** oldVars : Variables ** Initialize nm oldVars newVars)

genInitializeOld' : Fuel -> (oldVars : Variables) -> Gen (nm : Nat ** newVars : Variables ** Initialize nm oldVars newVars)

-- Definitions of auxillary generators
genExistsOfType' fl = genExistsOfType @{genVariableDoesNotExistVar}
                                      @{genExistsOfTypeVars}
                                      fl


genExpressionAll' fl = genExpressionAll @{genInt}
                                        @{genJType}
                                        @{genExistsOfTypeAll}
                                        @{genExistsOfTypeJTyVars}
                                        fl

genExpressionVars' fl = genExpressionVars @{genInt}
                                          @{genJType}
                                          fl

genExpression' fl = genExpression @{genInt}
                                  @{genJType}
                                  @{genVariableDoesNotExistVar}
                                  @{genVariableDoesNotExistVars}
                                  @{genVariableDoesNotExist}
                                  fl


genInitialize' fl = genInitialize @{genVariableDoesNotExistVar}
                                  @{genVariableDoesNotExistAll}
                                  @{genVariableDoesNotExistVars}
                                  fl

genInitializeNameOld' fl = genInitializeNameOld @{genVariableDoesNotExistAll}
                                                @{genVariableDoesNotExistVar}
                                                @{genInitializeAll}
                                                fl

genInitializeNew' fl = genInitializeNew @{genVariableDoesNotExistAll}
                                        fl

genInitializeOld' fl = genInitializeOld @{genVariableDoesNotExistAll}
                                        fl

-- Statement generator
genStatementAll' : Fuel -> (vars : Variables) -> Gen $ Statement vars
genStatementAll' fl = genStatementAllGiven  @{genJType}
                                            @{genInt}

                                            @{genExistsOfTypeVars}
                                            @{genExistsOfTypeNameVars}
                                            @{genExistsOfTypeJTyVars}
                                            @{genExistsOfTypeAll}
                                            @{genExistsOfType'}

                                            @{genExpressionAll'}
                                            @{genExpressionVars'}
                                            @{genExpression'}

                                            @{genVariableDoesNotExistVar}
                                            @{genVariableDoesNotExist}

                                            @{genInitialize'}
                                            @{genInitializeNameOld'}
                                            @{genInitializeOldNew}
                                            @{genInitializeAll}
                                            @{genInitializeNew'}
                                            @{genInitializeOld'}
                                            fl

genMainClass : Fuel -> Gen $ MainClass
genMainClass fl = MkMain "MainClass" <$> (genStatementAll' fl [MkVar 1 JBool Init, MkVar 0 JBool Init])

export
genProgram : Fuel -> Gen $ Program
genProgram fuel = MkProgram <$> genMainClass fuel

