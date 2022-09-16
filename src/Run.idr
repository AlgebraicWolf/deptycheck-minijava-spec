module Run

import Data.Fin
import Data.Fuel
import Data.String
import Data.List
import Data.List.Lazy
import Data.Maybe
import Data.Either
import Test.DepTyCheck.Gen
import Control.Monad.State
import Spec.Class
import Spec.Expression
import System
import System.File
import System.Directory
import Control.App
import Control.App.Console
import Control.App.FileIO

import Gens
import Mapper
import ShowInstances

lazy_for : Monad m => LazyList a -> (a -> m Unit) -> m Unit
lazy_for xs f = foldrLazy ((>>) . f) (pure ()) xs

-- runOnce : (variant : Nat) -> Gen a -> LazyList a
-- runOnce v gen = evalState (fst $ next someStdGen) (unGen $ variant v gen)

someValue : Nat -> Gen a -> Maybe a
someValue n gen = head' $ unGenTryN 100000000 someStdGen $ variant n $ gen

checkNat : Integer -> Maybe Nat
checkNat n = toMaybe (n >= 0) (integerToNat n)

prependMaybe : Maybe a -> List a -> List a
prependMaybe Nothing xs = xs
prependMaybe (Just x) xs = x::xs


toFileEx : FileError -> FileEx
toFileEx (GenericFileError i) = GenericFileEx i
toFileEx FileReadError = FileReadError
toFileEx FileWriteError = FileWriteError
toFileEx FileNotFound = FileNotFound
toFileEx PermissionDenied = PermissionDenied
toFileEx FileExists = FileExists


data GenericError = MkGenericError String

Show GenericError where
  show (MkGenericError msg) = msg

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

Show OptType where
  show (RequiredStr x) = "<" ++ x ++ ">"
  show (RequiredNat x) = "<" ++ x ++ ">"

ActType : Maybe OptType -> Type
ActType Nothing = CLParam
ActType (Just (RequiredStr x)) = String -> CLParam
ActType (Just (RequiredNat x)) = Nat -> CLParam

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

optShow : OptDesc -> (String, Maybe String)
optShow (MkOpt [] _ _ _) = ("", Just "")
optShow (MkOpt flags arg action help) = (showSep ", " flags ++ " " ++ showMaybe (show <$> arg), help)  where
  showSep : String -> List String -> String
  showSep sep [] = ""
  showSep sep [x] = x
  showSep sep (x :: xs) = x ++ sep ++ showSep sep xs

  showMaybe : Maybe String -> String
  showMaybe Nothing = ""
  showMaybe (Just x) = x

fstWidth : List (String, a) -> Nat
fstWidth rows = foldr max 0 $ map (length . fst) rows

textFromOptions : List OptDesc -> String
textFromOptions opts = let rows = optShow <$> opts in
                       let width = fstWidth rows in
                           concatMap (showRow width) rows where
  showRow : Nat -> (String, Maybe String) -> String
  showRow maxWidth (optshow, help) = maybe ""
                                          (\h => "  " ++ optshow ++ pack (replicate (minus (maxWidth + 2) (length optshow)) ' ') ++ h ++ "\n")
                                          help

-- Tags for parameters
data OutDir : Type where
data NumTests : Type where
data Stride : Type where
data NumFuel : Type where
data HelpOnly : Type where

data AppConfig : Type where

record Config where
  constructor MkConfig
  outDir : Maybe String
  numTests : Nat
  stride : Nat
  numFuel : Fuel
  helpOnly : Bool

defaultConfig : Config
defaultConfig = MkConfig Nothing
                         30
                         1
                         (limit 4)
                         False

processArgs : String -> (args : Maybe OptType) -> List String -> ActType args -> Either String (Maybe (CLParam, List String))
processArgs flag Nothing xs f = Right $ Just (f, xs)
processArgs flag (Just opt@(RequiredStr x)) [] f = Left $ "Missing required argument " ++ show opt ++ " for flag " ++ flag
processArgs flag (Just opt@(RequiredNat x)) [] f = Left $ "Missing required argument " ++ show opt ++ " for flag " ++ flag
processArgs flag (Just opt@(RequiredStr x)) (y :: xs) f = Right $ Just (f y, xs)
processArgs flag (Just opt@(RequiredNat x)) (y :: xs) f = do
  arg <- maybeToEither ("Expected Nat argument " ++ show y ++ " for flag " ++ flag) (parseInteger y >>= checkNat)
  pure $ Just (f arg, xs)

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


-- printOnce : (n : Nat) -> Gen Program -> IO Unit
-- printOnce n gen = lazy_for (iterateN n S Z) $ \v => do
--   print "\n==========\n"
--   let (x::_) = runOnce v gen
--     | [] => print "Generator is empty"
--   print $ programToCode x
--   where
--     print : String -> IO Unit
--     print str = putStrLn str >> fflush stdout

processArg : State AppConfig Config es => CLParam -> App es ()
processArg Help = do conf <- get AppConfig
                     put AppConfig ({ helpOnly := True } conf)
processArg (OutputDir str) = do conf <- get AppConfig
                                put AppConfig ({ outDir := Just str} conf)
processArg (NTests k) = do conf <- get AppConfig
                           put AppConfig ({ numTests := k} conf)
processArg (NSkip k) = do conf <- get AppConfig
                          put AppConfig ({ stride := k} conf)
processArg (NFuel k) = do conf <- get AppConfig
                          put AppConfig ({ numFuel := limit k} conf)

showHelp : Console es => App es ()
showHelp = putStr $ textFromOptions options

writeTest : Has [Console, FileIO] es => String -> Nat -> Nat -> Program -> App es ()
writeTest dir tot n prog = do
  let path = dir ++ "/" ++ "test" ++ show n
  let test_path = path ++ ".java"
  let test_minijava_path = path ++ ".mjava"
  let raw_term_path = path ++ ".term"
  let oracle_path = path ++ ".json"
  putStrLn $ "Saving test " ++ show (S n) ++ "/" ++ show tot
  withFile test_path WriteTruncate
    throw
    (\f => fPutStr f $ programToCode prog)
  withFile test_minijava_path WriteTruncate
    throw
    (\f => fPutStr f $ programToMiniJavaCode prog)
  withFile raw_term_path WriteTruncate
    throw
    (\f => fPutStr f $ show prog)
  withFile oracle_path WriteTruncate
    throw
    (\f => fPutStr f $ programToOracle prog)

softInit : List a -> List a
softInit xs = maybe [] id $ init' xs

writeMetadata : Has [FileIO, State AppConfig Config] es => String -> App es ()
writeMetadata dir = do
  let file_path = dir ++ "/settings.json"
  let empty_path = dir ++ "/empty"

  conf <- get AppConfig

  withFile file_path WriteTruncate throw $ \f => do
    fPutStrLn f $ "{"
    fPutStrLn f $ "    \"executables\": [\"javac\", \"java\", \"thirdparty/MiniJava_Interpreter/build/mini_java\"],"
    fPutStrLn f $ "    \"stages\": [\"oracle_compile\", \"oracle_run\", \"interpreter\"],"
    fPutStrLn f $ "    \"oracle_compile\": \"javac " ++ dir ++ "/{TESTNAME}.java\","
    fPutStrLn f $ "    \"oracle_run\": \"java -classpath " ++ dir ++ " {CLASSNAME}\","
    fPutStrLn f $ "    \"interpreter\": \"thirdparty/MiniJava_Interpreter/build/mini_java " ++ dir ++ "/{TESTNAME}.mjava\","
    fPutStrLn f $ "    \"tests\": " ++ show ([ "test" ++ show i | i <- softInit [0..conf.numTests] ])
    fPutStrLn f $ "}"

  withFile empty_path WriteTruncate throw (\_ => pure ())

fileOp : Has [PrimIO, Exception IOError] es => IO (Either FileError a) -> App es a
fileOp fileRes = do Right res <- primIO fileRes
                      | Left err => throw $ FileErr $ toFileEx err
                    pure res

createDir : Has [PrimIO, Exception IOError] es => String -> App es ()
createDir = fileOp . createDir

removeFile : Has [PrimIO, Exception IOError] es => String -> App es ()
removeFile = fileOp . removeFile

eachNth : Nat -> LazyList a -> LazyList a
eachNth = eachNth' 0 where
  eachNth' : Nat -> Nat -> LazyList a -> LazyList a
  eachNth' k n [] = []
  eachNth' 0 n (x :: xs) = x :: (eachNth' n n xs)
  eachNth' (S k) n (x :: xs) = eachNth' k n xs

someValue' : Nat -> Gen a -> a

generateTests : Has [PrimIO, Console, State AppConfig Config, FileIO] es => String -> Gen Program -> App es ()
generateTests path gen = do
  conf <- get AppConfig
  -- TODO Handle case when the directory is already present
  createDir path
  writeMetadata path

  lazy_for (iterateN conf.numTests S Z) $ \v => do
    let maybeProg = someValue (conf.stride + v) gen
    case maybeProg of
      (Just prog) => writeTest path conf.numTests v prog
      Nothing => putStrLn "Nothing generated"


mainApp : Has [PrimIO, State AppConfig Config, FileIO, Exception GenericError, Console] es => List String -> App es ()
mainApp args = do
  arglist <- processOpts args
  for_ arglist processArg
  conf <- get AppConfig
  if conf.helpOnly
    then do
      showHelp
    else do
      case conf.outDir of
        Nothing => throw $ MkGenericError "Output directory was not specified"
        (Just path) => generateTests path $ genProgram $ conf.numFuel
  pure ()

mainAppInitVars : Has [PrimIO, FileIO, Exception GenericError, Console] es => List String -> App es ()
mainAppInitVars args = new defaultConfig $ mainApp args

mainAppNoexcept : Console es => Console (IOError :: es) => PrimIO (GenericError :: IOError :: es) => List String -> App es ()
mainAppNoexcept args = let mainArgs = mainAppInitVars args in
                       let h1 = handle mainArgs
                                       pure
                                       (\err : GenericError => putStrLn $ "Error: " ++ show err) in
                       let h2 = handle h1
                                       pure
                                       (\err : IOError => putStrLn $ "Error: " ++ show err) in
                                       h2

main : IO Unit
main = do
  args' <- getArgs
  case args' of
    [] => putStrLn "Argument list is empty for some bizzare reason"
    (_::args) => run $ mainAppNoexcept args

