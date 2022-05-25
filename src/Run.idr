module Run

import Test.DepTyCheck.Gen
import Spec.Class
import Gens

-- create_variable : Statement [] ?
-- create_variable = VarDeclaration JInt "x"

assign_main : Statement [] ?
assign_main = Compose (VarDeclaration JInt "x") (Assignment "x" (IntegerLiteral 5))

test : Program
test = MkProgram $ MkMain "Main" assign_main

main : IO Unit
main = do
  putStrLn "Example of preprogrammed test rendering:"
  print test
