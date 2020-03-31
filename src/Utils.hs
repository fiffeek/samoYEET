module Utils where
import           ValueTypes

returnProgram :: VType -> String
returnProgram VNone = "--- Program didn't return any value"
returnProgram ret =
  (showString "--- Program returned = [")
    . (shows ret)
    . (showString "]\n")
    $ ""
