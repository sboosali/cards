{-| This module is a stub for development.

see <http://neilmitchell.blogspot.com/2008/02/adding-data-files-using-cabal.html>

''While developing the program our hand-created Paths module will be invoked, which says the data is always in the current directory. When doing a Cabal build, Cabal will choose its custom generated Paths module over ours, and we get the benefits of Cabal managing our data.''

The presence of this module solves:

@
  Exception when trying to run compile-time code:
          /home/sboo/.cabal/share/x86_64-linux-ghc-8.0.2/cards-common-0.0.0/data/json/RIXSets-z.json: openBinaryFile: does not exist (No such file or directory)
@

-}
module Paths_cards_common where
import Prelude

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
