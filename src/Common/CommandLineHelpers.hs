module Common.CommandLineHelpers where
import           Data.List

class Explain a where
    explain :: a -> String

data CommandLineArgument = Help 
 | Debug { dEnabled :: Bool } 
 | Warnings { wEnabled :: Bool } 
 | Files {files :: [String] }
 | TypeCheck { tcEnabled :: Bool }
initDebug = Debug False
initWarning = Warnings False
initTypeCheck = TypeCheck True

instance Show CommandLineArgument where
  show (Debug    _) = "--debug"
  show (Warnings _) = "--warnings"
  show (Help      ) = "--help"
  show (TypeCheck _) = "--disable-type-check"
  show _            = ""

instance Explain CommandLineArgument where
  explain = go
   where
    indent = 15
    getIndent str = concat $ replicate (indent - (length str)) " "
    go d@(Debug _) =
      intercalate (getIndent . show $ d) [show d, "Enable debug logging."]
    go w@(Warnings _) =
      intercalate (getIndent . show $ w) [show w, "Enable warnings."]
    go h@(Help) =
      intercalate (getIndent . show $ h) [show h, "Display this help message."]
    go t@(TypeCheck _) =
      intercalate (getIndent . show $ t) [show t, "Disable type checking."]
    go f@(Files _) =
      intercalate (getIndent "[file]") ["[file]", "Parse content of files."]

data CommandLineArguments = CLA {debug :: CommandLineArgument
  , warnings :: CommandLineArgument
  , maybeHelp :: Maybe CommandLineArgument
  , fs :: CommandLineArgument
  , typeCheck :: CommandLineArgument }
parseArgs args = CLA
  { maybeHelp = fmap (\_ -> Help) (find ((show Help) ==) args)
  , debug     = Debug $ (show initDebug) `elem` args
  , warnings  = Warnings $ (show initWarning) `elem` args
  , typeCheck = TypeCheck . not $ (show initTypeCheck) `elem` args 
  , fs        = Files
                  $ filter (not . flip elem [show initDebug, show initWarning, show initTypeCheck]) args
  }

instance Explain CommandLineArguments where
  explain (CLA d w _ fs tc) =
    unlines
      $  ["usage: Call with one of the following argument combinations:"]
      ++ (map explain [d, w, Help, fs, tc])

isDebugEnabled :: CommandLineArguments -> Bool
isDebugEnabled cla = go $ debug cla
 where
  go (Debug True) = True
  go _            = False

isTypeCheckEnabled :: CommandLineArguments -> Bool
isTypeCheckEnabled cla = go $ typeCheck cla
 where
   go (TypeCheck True) = True
   go _ = False

getFiles :: CommandLineArguments -> [String]
getFiles cla = go $ fs cla
 where
  go (Files files) = files
  go _             = []
