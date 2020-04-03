module Common.CommandLineHelpers where
import           Data.List

class Explain a where
    explain :: a -> String

data CommandLineArgument = Help | Debug { dEnabled :: Bool } | Warnings { wEnabled :: Bool } | Files {files :: [String] }
initDebug = Debug False
initWarning = Warnings False

instance Show CommandLineArgument where
  show (Debug    _) = "--debug"
  show (Warnings _) = "--warnings"
  show (Help      ) = "--help"
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
    go f@(Files _) =
      intercalate (getIndent "[file]") ["[file]", "Parse content of files."]




data CommandLineArguments = CLA {debug :: CommandLineArgument, warnings :: CommandLineArgument, maybeHelp :: Maybe CommandLineArgument, fs :: CommandLineArgument }
parseArgs args = CLA
  { maybeHelp = fmap (\_ -> Help) (find ((show Help) ==) args)
  , debug     = Debug $ (show initDebug) `elem` args
  , warnings  = Warnings $ (show initWarning) `elem` args
  , fs        = Files
                  $ filter (not . flip elem [show initDebug, show initWarning]) args
  }

instance Explain CommandLineArguments where
  explain (CLA d w _ fs) =
    unlines
      $  ["usage: Call with one of the following argument combinations:"]
      ++ (map explain [d, w, Help, fs])

isDebugEnabled :: CommandLineArguments -> Bool
isDebugEnabled cla = go $ debug cla
 where
  go (Debug True) = True
  go _            = False

getFiles :: CommandLineArguments -> [String]
getFiles cla = go $ fs cla
 where
  go (Files files) = files
  go _             = []
