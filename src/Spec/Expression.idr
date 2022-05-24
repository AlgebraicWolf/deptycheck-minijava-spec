module Spec.Expression

import Data.List.Lookup

import public Spec.Aspects.Variables

-- For now, this is based on the MiniJava grammar
public export
data Expression : (vars : Variables) -> (result : JType) -> Type where
  BoolTrue : Expression vars JBool
  BoolFalse : Expression vars JBool
  IntegerLiteral : Int -> Expression vars JInt
  FromIdentifier : (n : Identifier) -> (0 lk : Lookup n vars) => Expression vars lk.reveal

export
Show (Expression vars res) where
  show BoolTrue = "true"
  show BoolFalse = "false"
  show (IntegerLiteral x) = show x
  show (FromIdentifier n) = show n

