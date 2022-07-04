module Spec.Expression

import Data.Fin
import public Spec.Aspects.Variables

-- For now, this is based on the MiniJava grammar
public export
data Expression : (n : Nat) -> (vars : Variables n) -> (result : JType) -> Type where
  BoolTrue : Expression n vars JBool
  BoolFalse : Expression n vars JBool
  IntegerLiteral : Int -> Expression n vars JInt
  FromIdentifier : (k : Fin n) -> IsOfType n k jty vars => Expression n vars jty

export
Show (Expression n vars res) where
  show BoolTrue = "true"
  show BoolFalse = "false"
  show (IntegerLiteral x) = show x
  show (FromIdentifier k) = "x" ++ show (finToNat k)

