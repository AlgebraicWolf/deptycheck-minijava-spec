module Mapper

import Data.Fin

import Spec.Class

typeToCode : JType -> String
typeToCode JBool = "boolean"
typeToCode JInt = "int"

exprToCode : {n : Nat} -> {vars : Variables n} -> Expression n vars res -> String
exprToCode BoolTrue = "true"
exprToCode BoolFalse = "false"
exprToCode (IntegerLiteral x) = show x
exprToCode {n = (S m)} (FromIdentifier k) = "x" ++ show (the (Fin (S m)) last - k)

stmtToCode : {n : Nat} -> {vars : Variables n} -> Statement n vars -> String
stmtToCode = (foldr ((++) . (++ "\n")) "") . reverse . stmtToCode' where
  stmtToCode' : {n : Nat} -> {vars : Variables n} -> Statement n vars -> List String
  stmtToCode' {n = S k} (VarDeclaration type stmt) = (typeToCode type ++ " x" ++ show k ++ ";") ::(stmtToCode' stmt)
  stmtToCode' (Assignment k expr stmt) = ("x" ++ show (complement k) ++ " = " ++ exprToCode expr ++ ";")::(stmtToCode' stmt)
  stmtToCode' Empty = []

mainClassToCode : MainClass -> String
mainClassToCode (MkMain _ main) = "class Main {\npublic static void main(String[] args) {\n" ++ stmtToCode main ++ "}\n}\n"

export
programToCode : Program -> String
programToCode (MkProgram x) = mainClassToCode x

