{-# LANGUAGE TemplateHaskell #-}

{-|

imports @Paths_cards_common@. 

see <http://neilmitchell.blogspot.com/2008/02/adding-data-files-using-cabal.html>

''The Extra-Source-Files tells Cabal to put the files in the release tarball, but nothing more - for a readme this behaviour is perfect. The Data-Files section tells Cabal that the following files contain data which the program will want to access at runtime. Data files include things like big tables, the hoogle function search database, graphics/game data files for games, UI description files for GUI's, etc.''



-}
module MTGJSON.Paths where

import Paths_cards_common

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)

import Data.FileEmbed

import Language.Haskell.TH (ExpQ, runIO)
import Language.Haskell.TH.Syntax (qAddDependentFile)

import Prelude.Spiros --((&))

----------------------------------------

-- | known data (in the @data-files: ...@ stanza of the @.cabal@).  
data DATA_FILE
  = AllDataFile
  | CardsDataFile
  | SetsDataFile
  | VersionDataFile
  deriving (Show)

----------------------------------------

-- | relative filepath for a @data-file@.
relativeDataFilePath :: DATA_FILE -> FilePath
relativeDataFilePath = \case
  AllDataFile     -> "data/json/AllSets-x.json"
  CardsDataFile   -> "data/json/RealSets-x.json"
  SetsDataFile    -> "data/json/SetList.json"
  VersionDataFile -> "data/json/version.json"
  
-- | absolute filepath for a @data-file@.
absoluteDataFilePath :: DATA_FILE -> IO FilePath
absoluteDataFilePath = relativeDataFilePath > getDataFileName

readDataFile :: DATA_FILE -> IO ByteString
readDataFile = absoluteDataFilePath >=> B.readFile

---------------------------------------

{-| wraps 'getDataFileName' and 'embedFile'.

use as a macro.

-}
embedDataFile
  :: DATA_FILE 
  -> ExpQ
embedDataFile datafile = do
  p <- runIO $ absoluteDataFilePath datafile 
  qAddDependentFile p
  embedFile p

----------------------------------------
