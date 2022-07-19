module Run

import Data.Fin
import Data.Fuel
import Data.List.Lazy
import Test.DepTyCheck.Gen
import Control.Monad.State
import Spec.Class
import Spec.Expression
import Gens
import Mapper
import System.File.Process
import System.File.Virtual

-- create_variable : Statement [] ?
-- create_variable = VarDeclaration JInt "x"

-- assign_main : Statement ? ?
-- assign_main = Assignment 1 BoolTrue (Assignment 0 (IntegerLiteral 5) (VarDeclaration JInt (VarDeclaration JBool Empty)))

-- test : Program
-- test = MkProgram $ MkMain "Main" assign_main

lazy_for : Monad m => LazyList a -> (a -> m Unit) -> m Unit
lazy_for xs f = foldrLazy ((>>) . f) (pure ()) xs

runOnce : (variant : Nat) -> Gen a -> LazyList a
runOnce v gen = evalState (fst $ next someStdGen) (unGen $ variant v gen)

printOnce : (n : Nat) -> Gen Program -> IO Unit
printOnce n gen = lazy_for (iterateN n S Z) $ \v => do
  print "\n==========\n"
  let (x::_) = runOnce v gen
    | [] => print "Generator is empty"
  print $ programToCode x
  where
    print : String -> IO Unit
    print str = putStrLn str >> fflush stdout

main : IO Unit
main = do
  putStrLn "Program generation"
  printOnce 30 $ genProgram $ limit 5
