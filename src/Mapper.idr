module Mapper

import Data.Fin

import Spec.Class

typeToCode : JType -> String
typeToCode JBool = "boolean"
typeToCode JInt = "int"

exprToCode : Expression vars res -> String
exprToCode BoolTrue = "true"
exprToCode BoolFalse = "false"
exprToCode (IntegerLiteral x) = show x
exprToCode (FromIdentifier name) = "x" ++ show name

stmtToCode : Statement vars -> String
stmtToCode = (foldr ((++) . (++ "\n")) "") . reverse . stmtToCode' where
  stmtToCode' : Statement v -> List String
  stmtToCode' (VarDeclaration type name stmt) = (typeToCode type ++ " x" ++ show name ++ ";")::(stmtToCode' stmt)
  stmtToCode' (Assignment _ name stmt (MkAssignmentExpressionWrap _ name jty expr x) _) = ("x" ++ show name ++ " = " ++ exprToCode expr ++ ";")::(stmtToCode' stmt)

  stmtToCode' (Print expr stmt) = ("System.out.println(" ++ exprToCode expr ++ ");")::(stmtToCode' stmt)
  stmtToCode' (Block inside _ stmt) = ("{\n" ++ stmtToCode inside ++ "}")::(stmtToCode' stmt)
  stmtToCode' Empty = []

mainClassToCode : MainClass -> String
mainClassToCode (MkMain name main) = "class " ++ show name ++ " {\npublic static void main(String[] args) {\n" ++ stmtToCode main ++ "}\n}\n"

mainClassToMiniJavaCode : MainClass -> String
mainClassToMiniJavaCode (MkMain name main) = "class " ++ show name ++ " {\npublic static void main() {\n" ++ stmtToCode main ++ "}\n}\n"

export
programToCode : Program -> String
programToCode (MkProgram x) = mainClassToCode x

export
programToMiniJavaCode : Program -> String
programToMiniJavaCode (MkProgram x) = mainClassToMiniJavaCode x

export
programToOracle : Program -> String
programToOracle (MkProgram (MkMain name main)) =  "{\n"
                                               ++ "   \"oracle_compile\": {\n"
                                               ++ "      \"returned\": \"0\",\n"
                                               ++ "      \"stderr\": {\n"
                                               ++ "         \"type\": \"compare\",\n"
                                               ++ "         \"value\": \"empty\"\n"
                                               ++ "      },\n"
                                               ++ "      \"stdout\": {\n"
                                               ++ "         \"type\": \"compare\",\n"
                                               ++ "         \"value\": \"empty\"\n"
                                               ++ "      }\n"
                                               ++ "   \},\n"
                                               ++ "   \"oracle_run\": {\n"
                                               ++ "      \"returned\": \"0\",\n"
                                               ++ "      \"stderr\": {\n"
                                               ++ "         \"type\": \"compare\",\n"
                                               ++ "         \"value\": \"empty\"\n"
                                               ++ "      },\n"
                                               ++ "      \"stdout\": {\n"
                                               ++ "         \"type\": \"write\",\n"
                                               ++ "         \"value\": \"output\"\n"
                                               ++ "      }\n"
                                               ++ "   \},\n"
                                               ++ "   \"interpreter\": {\n"
                                               ++ "      \"returned\": \"0\",\n"
                                               ++ "      \"stderr\": {\n"
                                               ++ "         \"type\": \"compare\",\n"
                                               ++ "         \"value\": \"empty\"\n"
                                               ++ "      },\n"
                                               ++ "      \"stdout\": {\n"
                                               ++ "         \"type\": \"compare\",\n"
                                               ++ "         \"value\": \"output\"\n"
                                               ++ "      }\n"
                                               ++ "   },\n"
                                               ++ "   \"vars\": {\n"
                                               ++ "      \"CLASSNAME\": \"" ++ show name ++ "\"\n"
                                               ++ "   }\n"
                                               ++ "}\n"

