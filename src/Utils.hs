module Utils where
import           ValueTypes

returnProgram :: VType -> String
returnProgram ret =
  (showString "--- Program returned = [")
    . (shows ret)
    . (showString "]\n")
    $ ""
