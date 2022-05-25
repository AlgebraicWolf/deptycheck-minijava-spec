module Mapper

import Data.Fin

import Spec.Class

exprToCode : {n : Nat} -> {vars : Variables n} -> Expression n vars res -> String
exprToCode BoolTrue = "true"
exprToCode BoolFalse = "false"
exprToCode (IntegerLiteral x) = show x
exprToCode (FromIdentifier k) = "x" ++ show (last - weaken k)

