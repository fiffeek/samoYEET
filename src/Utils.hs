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
