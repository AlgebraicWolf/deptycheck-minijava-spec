module Spec.Class

import public Spec.Statement

%default total

-- For now, the main class shall contain the only method "main"
public export
data MainClass : Type where
  -- There are no global variables yet. Thus, statement should not expect any variables to be available
  MkMain : {vars : Variables} -> (n : Identifier) -> (main : Statement [] vars) -> MainClass

-- For now, the wrapper is useless, but I left it for expansion purposes
public export
data Program : Type where
  MkProgram : MainClass -> Program

-- export
-- Show MainClass where
--   show (MkMain n main) = "class " ++ show n ++ " {\npublic static void main(String[] args) {\n" ++ show main ++"}\n}\n"

-- export
-- Show Program where
--   show (MkProgram x) = show x


