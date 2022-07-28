module Spec.Expression

import Data.Fin
import public Spec.Aspects.Variables

%default total

-- For now, this is based on the MiniJava grammar
public export
data Expression : (vars : Variables) -> (result : JType) -> Type where
  BoolTrue : Expression vars JBool
  BoolFalse : Expression vars JBool
  IntegerLiteral : Int -> Expression vars JInt
  FromIdentifier : (k : Nat) -> IsOfType k jty vars => IsInit k vars => Expression vars jty

export
Show (Expression vars res) where
  show BoolTrue = "true"
  show BoolFalse = "false"
  show (IntegerLiteral x) = show x
  show (FromIdentifier k) = "x" ++ show k

