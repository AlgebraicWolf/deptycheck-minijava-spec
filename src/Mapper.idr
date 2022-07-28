module Mapper

import Data.Fin

import Spec.Class

typeToCode : JType -> String
typeToCode JBool = "boolean"
typeToCode JInt = "int"

exprToCode : {vars : Variables} -> Expression vars res -> String
exprToCode BoolTrue = "true"
exprToCode BoolFalse = "false"
exprToCode (IntegerLiteral x) = show x
exprToCode (FromIdentifier k) = "x" ++ show (minus (length vars) k)

stmtToCode : {vars : Variables} -> Statement vars -> String
stmtToCode = (foldr ((++) . (++ "\n")) "") . reverse . stmtToCode' where
  stmtToCode' : {v : Variables} -> Statement v -> List String
  stmtToCode' (VarDeclaration type stmt) = (typeToCode type ++ " x" ++ show (minus (length v) 1) ++ ";")::(stmtToCode' stmt)
  stmtToCode' (Assignment vars' k expr stmt) = ("x" ++ show (complement k) ++ " = " ++ exprToCode expr ++ ";")::(stmtToCode' stmt)
  stmtToCode' Empty = []

mainClassToCode : MainClass -> String
mainClassToCode (MkMain name main) = "class " ++ show name ++ " {\npublic static void main(String[] args) {\n" ++ stmtToCode main ++ "}\n}\n"

export
programToCode : Program -> String
programToCode (MkProgram x) = mainClassToCode x

export
programToOracle : Program -> String
programToOracle (MkProgram (MkMain name main)) =  "{\n"
                                               ++ "   \"compile\": {\n"
                                               ++ "      \"returned\": \"0\",\n"
                                               ++ "      \"stderr\": \"\",\n"
                                               ++ "      \"stdout\": \"\"\n"
                                               ++ "   \},\n"
                                               ++ "   \"run\": {\n"
                                               ++ "      \"returned\": \"0\",\n"
                                               ++ "      \"stderr\": \"\",\n"
                                               ++ "      \"stdout\": \"\"\n"
                                               ++ "   \},\n"
                                               ++ "   \"vars\": {\n"
                                               ++ "      \"CLASSNAME\": \"" ++ show name ++ "\"\n"
                                               ++ "   }\n"
                                               ++ "}\n"

