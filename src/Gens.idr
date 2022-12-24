module Gens

import Data.Fin
import Data.Fuel
import Data.List1
import Decidable.Equality.Core

import Test.DepTyCheck.Gen

import public Spec.Class

import Gens.Derived
import Gens.Manual

%default total

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


genVariableDoesNotExistVar' : Fuel -> (var : Variable) -> Gen (vars : Variables ** VariableDoesNotExist var vars)

genVariableDoesNotExistVars' : Fuel -> (vars : Variables) -> Gen (var : Variable ** VariableDoesNotExist var vars)

genVariableDoesNotExist' : Fuel -> Gen (var : Variable ** vars : Variables ** VariableDoesNotExist var vars)

genVariableDoesNotExistAll' : Fuel -> (var : Variable) -> (vars : Variables) -> Gen $ VariableDoesNotExist var vars


genInitialize' : Fuel -> Gen (nm : Nat ** oldVars : Variables ** newVars : Variables ** Initialize nm oldVars newVars)

genInitializeNameOld' : Fuel -> (nm : Nat) -> (oldVars : Variables) -> Gen (newVars : Variables ** Initialize nm oldVars newVars)

genInitializeNew' : Fuel -> (newVars : Variables) -> Gen (nm : Nat ** oldVars : Variables ** Initialize nm oldVars newVars)

genInitializeOld' : Fuel -> (oldVars : Variables) -> Gen (nm : Nat ** newVars : Variables ** Initialize nm oldVars newVars)

genNameDoesNotExist' : Fuel -> Gen (nm : Nat ** vars : Variables ** NameDoesNotExist nm vars)

genNameDoesNotExistVar' : Fuel -> (nm : Nat) -> Gen (vars : Variables ** NameDoesNotExist nm vars)

genNameDoesNotExistVars' : Fuel -> (vars : Variables) -> Gen (nm : Nat ** NameDoesNotExist nm vars)

genNameDoesNotExistAll' : Fuel -> (nm : Nat) -> (vars : Variables) -> Gen $ NameDoesNotExist nm vars

-- Definitions of auxillary generators
genNameDoesNotExist' fl = genNameDoesNotExist @{genNat}
                                              @{genNatNotEqualAll}
                                              @{genNatNotEqual0}
                                              @{genNatNotEqual1}
                                              @{genNatNotEqual}
                                              fl
genNameDoesNotExistVar' fl = genNameDoesNotExistVar @{genNat}
                                                    @{genNatNotEqualAll}
                                                    @{genNatNotEqual0}
                                                    @{genNatNotEqual1}
                                                    @{genNatNotEqual}
                                                    fl

genNameDoesNotExistVars' fl = genNameDoesNotExistVars @{genNat}
                                                      @{genNatNotEqualAll}
                                                      @{genNatNotEqual0}
                                                      @{genNatNotEqual1}
                                                      @{genNatNotEqual}
                                                      fl

genNameDoesNotExistAll' fl = genNameDoesNotExistAll @{genNat}
                                                    @{genNatNotEqualAll}
                                                    @{genNatNotEqual0}
                                                    @{genNatNotEqual1}
                                                    @{genNatNotEqual}
                                                    fl

genExistsOfType' fl = genExistsOfType @{genNat}
                                      @{genNameDoesNotExist'}
                                      @{genVariableDoesNotExistVar'}
                                      @{\fl => genExistsOfTypeVars fl @{genNat}}
                                      fl


genExpressionAll' fl = genExpressionAll @{genNat}
                                        @{genInt}
                                        @{genJType}
                                        @{\fl => genExistsOfTypeAll @{genNat} fl}
                                        @{\fl => genExistsOfTypeJTyVars @{genNat} fl}
                                        fl

genExpressionVars' fl = genExpressionVars @{genNat}
                                          @{genInt}
                                          @{genJType}
                                          fl

genExpression' fl = genExpression @{genNat}
                                  @{genInt}
                                  @{genJType}
                                  @{genVariableDoesNotExistVar'}
                                  @{genVariableDoesNotExistVars'}
                                  @{genVariableDoesNotExist'}
                                  fl


genInitialize' fl = genInitialize @{genNat}
                                  @{genVariableDoesNotExistVar'}
                                  @{genVariableDoesNotExistAll'}
                                  @{genVariableDoesNotExistVars'}
                                  fl

genInitializeNameOld' fl = genInitializeNameOld @{genNat}
                                                @{genVariableDoesNotExistAll'}
                                                @{genVariableDoesNotExistVar'}
                                                @{\fl => genInitializeAll fl @{genNat}}
                                                fl

genInitializeNew' fl = genInitializeNew @{genNat}
                                        @{genVariableDoesNotExistAll'}
                                        fl

genInitializeOld' fl = genInitializeOld @{genNat}
                                        @{genVariableDoesNotExistAll'}
                                        fl

genVariableDoesNotExistVar' fl = genVariableDoesNotExistVar fl @{genNat}
                                                               @{genNameDoesNotExistVar'}

genVariableDoesNotExistVars' fl = genVariableDoesNotExistVars fl @{genNat} @{genNameDoesNotExistVars'}

genVariableDoesNotExist' fl = genVariableDoesNotExist fl @{genNat} @{genNameDoesNotExist'}

genVariableDoesNotExistAll' fl = genVariableDoesNotExistAll fl @{genNat} @{genNameDoesNotExistAll'}

-- Statement generator
genStatementAll' : Fuel -> (preV : Variables) -> (postV : Variables) -> Gen $ Statement preV postV
genStatementAll' fl = genStatementAllGiven  @{genNat}
                                            @{genJType}
                                            @{genInt}

                                            @{\fl => genExistsOfTypeVars fl @{genNat}}
                                            @{\fl => genExistsOfTypeNameVars fl @{genNat}}
                                            @{\fl => genExistsOfTypeJTyVars fl @{genNat}}
                                            @{\fl => genExistsOfTypeAll fl @{genNat}}
                                            @{genExistsOfType'}

                                            @{genExpressionAll'}
                                            @{genExpressionVars'}
                                            @{genExpression'}

                                            @{genVariableDoesNotExistVar'}
                                            @{genVariableDoesNotExistVars'}
                                            @{genVariableDoesNotExist'}

                                            @{genNameDoesNotExist'}
                                            @{genNameDoesNotExistVars'}
                                            @{genNameDoesNotExistVar'}
                                            @{genNameDoesNotExistAll'}

                                            @{genInitialize'}
                                            @{genInitializeNameOld'}
                                            @{\fl => genInitializeOldNew fl @{genNat}}
                                            -- @{genInitializeAll}
                                            @{genInitializeNew'}
                                            @{genInitializeOld'}
                                            fl

-- genStatementWithBlock' : Fuel -> (vars : Variables) -> Gen $ StatementWithBlock vars
-- genStatementWithBlock' fl = genStatementWithBlock @{genJType}
--                                             @{genInt}

--                                             @{genExistsOfTypeVars}
--                                             @{genExistsOfTypeNameVars}
--                                             @{genExistsOfTypeJTyVars}
--                                             @{genExistsOfTypeAll}
--                                             @{genExistsOfType'}

--                                             @{genExpressionAll'}
--                                             @{genExpressionVars'}
--                                             @{genExpression'}

--                                             @{genVariableDoesNotExistVar'}
--                                             @{genVariableDoesNotExistVars'}
--                                             @{genVariableDoesNotExist'}

--                                             @{genInitialize'}
--                                             @{genInitializeNameOld'}
--                                             @{genInitializeOldNew}
--                                             @{genInitializeAll}
--                                             @{genInitializeNew'}
--                                             @{genInitializeOld'}
--                                             @{genStatementAll'}
--                                             fl


-- omitBlockProof : StatementWithBlock vars -> Statement vars
-- omitBlockProof (MkStatementWithBlock stmt _) = stmt

vars' : Variables
vars' = [MkVar 0 JBool Init]

vars : Variables
vars = (::) @{NameAvailable $ DeclDiff NoVars $ DiffName $ NotRefl (\case Refl impossible)} (MkVar 1 JBool Init) vars'

genMainClass : Fuel -> Gen $ MainClass
genMainClass fl = MkMain "MainClass" <$> genStatementAll' fl [] vars -- [MkVar 1 JBool Init, MkVar 0 JBool Init]
-- genMainClass fl = MkMain "MainClass" <$> omitBlockProof <$> genStatementWithBlock' fl [MkVar 1 JBool Init, MkVar 0 JBool Init]

export
genProgram : Fuel -> Gen $ Program
genProgram fuel = MkProgram <$> genMainClass fuel

