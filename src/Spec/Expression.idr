module Spec.Expression

import Data.Fin
import public Spec.Aspects.Variables

%default total

-- For now, this is based on the MiniJava grammar
public export
data Expression : (vars : Variables) -> (init : InitializedVariables) -> (result : JType) -> Type where
  FromIdentifier : (name : Nat) ->
  ExistsOfType name jty vars =>
  NameInitialized name init =>
  Expression vars init jty

  BoolTrue : Expression vars init JBool
  BoolFalse : Expression vars init JBool
  IntegerLiteral : Int -> Expression vars init JInt

export
Show (Expression vars init res) where
  show BoolTrue = "true"
  show BoolFalse = "false"
  show (IntegerLiteral x) = show x
  show (FromIdentifier k) = "x" ++ show k

