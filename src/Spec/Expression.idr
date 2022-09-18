module Spec.Expression

import Data.Fin
import public Spec.Variables
import Spec.NameInitialized

%default total

-- For now, this is based on the MiniJava grammar
public export
data Expression : (vars : Variables) -> (result : JType) -> Type where
  FromIdentifier : (name : Nat) ->
                   NameInitialized name vars => -- initialization implies existence
                   Expression vars jty

  BoolTrue : Expression vars JBool
  BoolFalse : Expression vars JBool
  IntegerLiteral : Int -> Expression vars JInt

export
Show (Expression vars res) where
  show BoolTrue = "true"
  show BoolFalse = "false"
  show (IntegerLiteral x) = show x
  show (FromIdentifier k) = "x" ++ show k

