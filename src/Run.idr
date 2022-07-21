module Run

import Data.Fin
import Data.Fuel
import Data.List.Lazy
import Test.DepTyCheck.Gen
import Control.Monad.State
import Spec.Class
import Spec.Expression
import System
import System.File
import Control.App
import Control.App.Console
import Control.App.FileIO

import Gens
import Mapper

lazy_for : Monad m => LazyList a -> (a -> m Unit) -> m Unit
lazy_for xs f = foldrLazy ((>>) . f) (pure ()) xs

runOnce : (variant : Nat) -> Gen a -> LazyList a
runOnce v gen = evalState (fst $ next someStdGen) (unGen $ variant v gen)

-- Enum of possible command-line options
data CLParam =
                -- Show help
                Help |
                -- Specify output directory for generated tests
                OutputDir String |
                -- Number of tests to generate
                NTests Nat |
                -- Number of generated tests to skip before pulling one out
                NSkip Nat |
                -- Fuel for generation
                NFuel Nat

data OptType = RequiredStr String
             | RequiredNat String
             | OptionalStr String
             | OptionalNat String

Show OptType where
  show (RequiredStr x) = "<" ++ x ++ ">"
  show (RequiredNat x) = "<" ++ x ++ ">"
  show (OptionalStr x) = "[" ++ x ++ "]"
  show (OptionalNat x) = "[" ++ x ++ "]"

ActType : Maybe OptType -> Type
ActType Nothing = CLParam
ActType (Just (RequiredStr x)) = String -> CLParam
ActType (Just (RequiredNat x)) = Nat -> CLParam
ActType (Just (OptionalStr x)) = Maybe String -> CLParam
ActType (Just (OptionalNat x)) = Maybe Int -> CLParam

record OptDesc where
  constructor MkOpt
  flags : List String
  arg : Maybe OptType
  action : ActType arg
  help : Maybe String

options : List OptDesc
options = [ MkOpt ["--help", "-h"] Nothing Help (Just "Show list of available options"),
            MkOpt ["--output-dir", "-o"] (Just $ RequiredStr "path") (\p => OutputDir p) (Just "Destination directory for generated tests"),
            MkOpt ["--ntests", "-n"] (Just $ RequiredNat "n") (\n => NTests n) (Just "Number of tests to generate"),
            MkOpt ["--skip", "-s"] (Just $ RequiredNat "n") (\n => NSkip n) (Just "Number of generated tests to skip before saving one"),
            MkOpt ["--fuel", "-f"] (Just $ RequiredNat "n") (\n => NFuel n) (Just "Amount of fuel to run generator") ]

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
