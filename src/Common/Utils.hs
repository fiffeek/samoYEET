module Common.Utils where
import           Common.CommandLineHelpers
import           Samoyeet.Lex
import           Samoyeet.Par
import           Samoyeet.Skel
import           Samoyeet.Print
import           Samoyeet.Abs
import           Samoyeet.ErrM
import           Common.Logging
import           Data.List

type IProgram = Program (Maybe (Int, Int))

showTree :: CommandLineArguments -> IProgram -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


showTextSeq :: [String] -> String
showTextSeq = concat . intersperse " "
