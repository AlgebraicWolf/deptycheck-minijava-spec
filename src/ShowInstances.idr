module ShowInstances

import Spec.Class

%language ElabReflection

showVariableDoesNotExist : VariableDoesNotExist var vars -> String

[debugJTyShow] Show JType where
  show JBool = "JBool"
  show JInt = "JInt"

Show (NameInitialized name vars) where
  show (Here @{prf}) = "Here @{" ++ showVariableDoesNotExist prf ++ "}"
  show (There x @{prf}) = "There (" ++ show x ++ ") @{" ++ showVariableDoesNotExist prf ++ "}"

[debugExprShow] Show (Expression vars jty) where
  show (FromIdentifier name @{initializedProof}) = "FromIdentifier " ++ show name ++ " @{" ++ show initializedProof ++ "}"
  show BoolTrue = "BoolTrue"
  show BoolFalse = "BoolFalse"
  show (IntegerLiteral x) = "IntegerLiteral " ++ show x

Show (NatNotEqual n m) where
  show SZ = "SZ"
  show ZS = "ZS"
  show (NotEqImpSNotEq x) = "NotEqImpSNotEq (" ++ show x ++ ")"

Show (NameDifferent nm var) where
  show (DiffName x) = "DiffName (" ++ show x ++ ")"

Show (NameDoesNotExist nm vars) where
  show NoVars = "NoVars"
  show (DeclDiff tail append @{nonExistencePrf}) = "DeclDiff (" ++ show tail ++ ") (" ++ show append ++ ") @{" ++ showVariableDoesNotExist nonExistencePrf ++ "}"

showVariableDoesNotExist (NameAvailable prf) = "NameAvailable (" ++ show prf ++ ")"

Show (VariableDoesNotExist var vars) where
  show = showVariableDoesNotExist

Show InitState where
  show NotInit = "NotInit"
  show Init = "Init"

Show Variable where
  show (MkVar nm jty init) = "MkVar " ++ show nm ++ " " ++ show @{debugJTyShow} jty ++ " " ++ show init

Show Variables where
  show [] = "[]"
  show ((::) var vars @{nonExistencePrf}) = "(::) (" ++ show var ++ ") (" ++ show vars ++ ") @{" ++ show nonExistencePrf ++ "}"

Show (ExistsOfType nm jty vars) where
  show (THere @{prf}) = "THere @{" ++ show prf ++ "}"
  show (TThere x @{nonExistencePrf}) = "TThere (" ++ show x ++ ") @{" ++ show nonExistencePrf ++ "}"

Show (AssignmentExpressionWrap nm vars) where
  show (MkAssignmentExpressionWrap vars nm jty expr prf) = "MkAssignmentExpressionWrap (" ++ show vars ++ ") " ++ show nm ++ " " ++ show @{debugJTyShow} jty ++ " (" ++ show @{debugExprShow} expr ++ ") (" ++ show prf ++ ")"

Show (Initialize nm oldVars newVars) where
  show (Here @{prf1} @{prf2}) = "Here @{" ++ show prf1 ++ "} @{" ++ show prf2 ++ "}"
  show (There x @{prf1} @{prf2}) = "There (" ++ show x ++ ") @{" ++ show prf1 ++ "} @{" ++ show prf2 ++ "}"

Show (Statement vars) where
  show (VarDeclaration jty name cont @{nonExistencePrf}) = "VarDeclaration " ++ show @{debugJTyShow} jty ++ " " ++ show name ++ " (" ++ show cont ++ ") @{" ++ show nonExistencePrf ++ "}"
  show (Assignment vars1 name cont wrap vars @{initializeFunc}) = "Assignment (" ++ show vars1 ++ ") " ++ show name ++ " (" ++ show cont ++ ") (" ++ show wrap ++ ") (" ++ show vars ++ ") @{" ++ show initializeFunc ++ "}"
  show (Print expr cont) = "Print (" ++ show cont ++ ") (" ++ show @{debugExprShow} expr ++ ")"
  show (Block inside newVars cont) = "Block (" ++ show inside ++ ") (" ++ show newVars ++ ") (" ++ show cont ++ ")"
  show Empty = "Empty"

Show MainClass where
  show (MkMain nm main) = "MkMain \"" ++ show nm ++ "\" (" ++ show main ++ ")"

export
Show Program where
  show (MkProgram x) = "MkProgram (" ++ show x ++ ")"

