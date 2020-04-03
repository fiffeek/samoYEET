module Common.Logging where
import           Common.CommandLineHelpers
import           Control.Monad                  ( when )

putStrV :: CommandLineArguments -> String -> IO ()
putStrV cla s = when (isDebugEnabled cla) $ putStrLn s
