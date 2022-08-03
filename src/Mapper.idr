module Mapper

import Data.Fin

import Spec.Class

typeToCode : JType -> String
typeToCode JBool = "boolean"
typeToCode JInt = "int"

exprToCode : {vars : Variables} -> Expression vars init res -> String
exprToCode BoolTrue = "true"
exprToCode BoolFalse = "false"
exprToCode (IntegerLiteral x) = show x
exprToCode (FromIdentifier name) = "x" ++ show name

stmtToCode : {vars : Variables} -> Statement vars init -> String
stmtToCode = (foldr ((++) . (++ "\n")) "") . reverse . stmtToCode' where
  stmtToCode' : {v : Variables} -> Statement v init' -> List String
  stmtToCode' (VarDeclaration type name stmt) = (typeToCode type ++ " x" ++ show name ++ ";")::(stmtToCode' stmt)
  stmtToCode' (Assignment vars' name _ expr stmt) = ("x" ++ show name ++ " = " ++ exprToCode expr ++ ";")::(stmtToCode' stmt)
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

