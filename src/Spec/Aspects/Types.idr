module Spec.Aspects.Types

import Decidable.Equality
import Decidable.Decidable

%default total

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
DecEq JType where
  decEq JBool JBool = Yes Refl
  decEq JInt JInt = Yes Refl
  decEq JBool JInt = No $ \case Refl impossible
  decEq JInt JBool = No $ \case Refl impossible

public export
Eq JType where
  x1 == x2 = isYes $ decEq x1 x2
