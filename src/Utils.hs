module Utils where
import           ValueTypes
import           Samoyeet.Abs

returnProgram :: VType -> String
returnProgram VNone = "--- Program didn't return any value"
returnProgram ret =
  (showString "--- Program returned = [")
    . (shows ret)
    . (showString "]\n")
    $ ""

splitOnArgType [] = ([], [])
splitOnArgType (a@((Arg _ _), _) : xs) =
  let (args, refs) = splitOnArgType xs in (a : args, refs)
splitOnArgType (((RefArg _ argName), (EVar calledWith)) : xs) =
  let (args, refs) = splitOnArgType xs in (args, (argName, calledWith) : refs)
