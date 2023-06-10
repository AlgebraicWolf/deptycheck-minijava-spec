module Spec.Expression

import Data.Fin
import public Spec.Variables
import public Spec.NameInitialized
import public Spec.ExistsOfType

%default total

-- A binary operation arguments and resulting type
public export
data BinaryOp : JType -> JType -> JType -> Type where
  -- Boolean logic
  And : BinaryOp JBool JBool JBool
  Or : BinaryOp JBool JBool JBool

  -- Arithmetic
  Add : BinaryOp JInt JInt JInt
  Subtract : BinaryOp JInt JInt JInt
  Mul : BinaryOp JInt JInt JInt
  Div : BinaryOp JInt JInt JInt
  Mod : BinaryOp JInt JInt JInt

  -- Order comparison
  LT : BinaryOp JInt JInt JBool
  LE : BinaryOp JInt JInt JBool
  GT : BinaryOp JInt JInt JBool
  GE : BinaryOp JInt JInt JBool

  -- Equality comparison
  EQ : BinaryOp t t JBool
  NEQ : BinaryOp t t JBool

-- An unary operation argument and resulting type
public export
data UnaryOp : JType -> JType -> Type where
  Not : UnaryOp JBool JBool

-- For now, this is based on the MiniJava grammar
public export
data Expression : (vars : Variables) -> (result : JType) -> Type where
  FromIdentifier : (name : Nat) ->
                   NameInitialized name vars => -- initialization implies existence
                   Expression vars jty
  BoolTrue : Expression vars JBool
  BoolFalse : Expression vars JBool
  IntegerLiteral : Int -> Expression vars JInt
  BinaryOperation : Expression vars lhs_ty -> Expression vars rhs_ty -> BinaryOp lhs_ty rhs_ty res_ty -> Expression vars res_ty
  -- UnaryOperation : Expression vars arg_ty -> UnaryOp arg_ty res_ty -> Expression vars res_ty

export
Show (BinaryOp lhs_ty rhs_ty res_ty) where
  show And = "&&"
  show Or = "||"
  show Add = "+"
  show Subtract = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show LT = "<"
  show LE = "<="
  show GT = ">"
  show GE = ">="
  show EQ = "=="
  show NEQ = "!="

export
Show (UnaryOp lhs_ty res_ty) where
  show Not = "!"

export
Show (Expression vars res) where
  show BoolTrue = "true"
  show BoolFalse = "false"
  show (IntegerLiteral x) = show x
  show (FromIdentifier k) = "x" ++ show k
  show (BinaryOperation lhs rhs op) = "(" ++ show lhs ++ ") " ++ show op ++ " (" ++ show rhs ++ ")"
