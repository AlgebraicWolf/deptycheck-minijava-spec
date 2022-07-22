module Run

import Data.Fin
import Data.Fuel
import Data.String
import Data.List.Lazy
import Data.Maybe
import Data.Either
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

checkNat : Integer -> Maybe Nat
checkNat n = toMaybe (n >= 0) (integerToNat n)

prependMaybe : Maybe a -> List a -> List a
prependMaybe Nothing xs = xs
prependMaybe (Just x) xs = x::xs

data GenericError = MkGenericError String

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

-- Tags for parameters
data OutDir : Type where
data NumTests : Type where
data Stride : Type where
data NumFuel : Type where
data HelpOnly : Type where

VarList : List (List Error -> Type)
VarList = [ State OutDir $ Maybe String
          , State NumTests Nat
          , State Stride Nat
          , State NumFuel Fuel
          , State HelpOnly Bool ]

processArgs : String -> (args : Maybe OptType) -> List String -> ActType args -> Either String (Maybe (CLParam, List String))
processArgs flag Nothing xs f = Right $ Just (f, xs)
processArgs flag (Just opt@(RequiredStr x)) [] f = Left $ "Missing required argument " ++ show opt ++ " for flag " ++ flag
processArgs flag (Just opt@(RequiredNat x)) [] f = Left $ "Missing required argument " ++ show opt ++ " for flag " ++ flag
processArgs flag (Just opt@(RequiredStr x)) (y :: xs) f = Right $ Just (f y, xs)
processArgs flag (Just opt@(RequiredNat x)) (y :: xs) f = do
  arg <- maybeToEither ("Expected Nat argument " ++ show y ++ " for flag " ++ flag) (parseInteger y >>= checkNat)
  pure $ Just (f arg, xs)

processArgs flag (Just opt@(OptionalStr x)) xs f = ?processArgs_rhs_4
processArgs flag (Just opt@(OptionalNat x)) xs f = ?processArgs_rhs_5

matchFlag : (d : OptDesc) -> List String -> Either String (Maybe (CLParam, List String))
matchFlag _ [] = Right Nothing
matchFlag d (x :: xs) = if x `elem` flags d
                          then processArgs x (arg d) xs (action d)
                          else Right Nothing

findMatch : List OptDesc -> List String -> Either String (Maybe CLParam, List String)
findMatch [] [] = Right (Nothing, [])
findMatch [] (x::args) = Left $ "Invalid parameter '" ++ x ++ "'"
findMatch (opt::opts) args = case !(matchFlag opt args) of
                                  Nothing => findMatch opts args
                                  (Just result) => Right $ mapFst Just result

parseOpts : List OptDesc -> List String -> Either String (List CLParam)
parseOpts opts [] = Right []
parseOpts opts args = do
  (cl, rest) <- findMatch opts args
  cls <- parseOpts opts rest
  pure $ cl `prependMaybe` cls

processOpts : Has [Exception GenericError] es => List String -> App es (List CLParam)
processOpts args = case parseOpts options args of
                        (Left err) => throw $ MkGenericError err
                        (Right x) => pure x


printOnce : (n : Nat) -> Gen Program -> IO Unit
printOnce n gen = lazy_for (iterateN n S Z) $ \v => do
  print "\n==========\n"
  let (x::_) = runOnce v gen
    | [] => print "Generator is empty"
  print $ programToCode x
  where
    print : String -> IO Unit
    print str = putStrLn str >> fflush stdout

processArg : Has VarList es => CLParam -> App es ()
processArg Help = put HelpOnly True
processArg (OutputDir str) = put OutDir $ Just str
processArg (NTests k) = put NumTests k
processArg (NSkip k) = put Stride k
processArg (NFuel k) = put NumFuel $ limit k

showHelp : Console es => App es ()
showHelp = ?showHelp_rhs

generateTests : Has [Console, FileIO] es => Has VarList es => String -> App es ()
generateTests path = ?generateTests_rhs

main_app : Has [Console, FileIO, Exception GenericError] es => Has VarList es => List String -> App es ()
main_app args = do
  arglist <- processOpts args
  for_ arglist processArg

  help <- get HelpOnly
  if help
    then do
      showHelp
    else do
      out_dir <- get OutDir
      n_tests <- get NumTests
      stride <- get Stride
      n_fuel <- get NumFuel
      case out_dir of
        Nothing => throw $ MkGenericError "Output directory was not specified"
        (Just path) => generateTests path
  pure ()

main : IO Unit
main = do
  args <- getArgs
  putStrLn "Program generation"
  printOnce 30 $ genProgram $ limit 5
