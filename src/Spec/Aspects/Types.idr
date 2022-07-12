module Spec.Aspects.Types

public export
data JType = JBool | JInt

public export
idrTypeOf : JType -> Type
idrTypeOf JBool = Bool
idrTypeOf JInt = Int

export
Show JType where
  show JBool = "boolean"
  show JInt = "int"

public export
Eq JType where
  JBool == JBool = True
  JInt == JInt = True
  x == y = False


