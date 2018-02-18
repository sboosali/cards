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
  = CardsDataFile
  | SetsDataFile
  | VersionDataFile
  deriving (Show)

----------------------------------------

-- | relative filepath for a @data-file@.
relativeDataFilePath :: DATA_FILE -> FilePath
relativeDataFilePath = \case
  CardsDataFile   -> "data/json/RealSets-y.json"
  SetsDataFile    -> "data/json/SetList.json"
  VersionDataFile -> "data/json/version.json"
  -- "data/json/AllSets-x.json"
  
-- | absolute filepath for a @data-file@.
absoluteDataFilePath :: DATA_FILE -> IO FilePath
absoluteDataFilePath = relativeDataFilePath > getDataFileName

readDataFile :: DATA_FILE -> IO ByteString
readDataFile = absoluteDataFilePath >=> B.readFile

---------------------------------------

{-| 'getDataFileName' plus 'embedFile'.

-}
embedDataFile
  :: FilePath 
  -- :: (FilePath -> IO ByteString)
  -- -> FilePath
  -> ExpQ
embedDataFile filepath = do
  fp <- runIO $ getDataFileName filepath
  -- no liftIO, i.e. `MonadIO Q`?
  qAddDependentFile fp
  embedFile fp

-- embedder :: FilePath -> ExpQ
-- embedder fp = do
--   qAddDependentFile fp
--   embedFile fp

----------------------------------------

-- embed'RIXSetsArray :: ExpQ
-- embed'RIXSetsArray = do
--   fp <- runIO getDataFileName'RIXSetsArray
--   -- no liftIO, i.e. `MonadIO Q`?
--   qAddDependentFile fp
--   embedFile fp

----------------------------------------

-- read'RIXSets :: IO ByteString
-- read'RIXSets = read'RIXSetsArray

-- read'RIXSetsArray :: IO ByteString
-- read'RIXSetsArray = getDataFileName'RIXSetsArray >>= B.readFile

-- ----------------------------------------

-- getDataFileName'RealSetsX :: IO FilePath
-- getDataFileName'RealSetsX = getDataFileName fp'RealSetsX

-- getDataFileName'RIXSetsArray :: IO FilePath
-- getDataFileName'RIXSetsArray = getDataFileName fp'RIXSetsArray

-- getDataFileName'RIXSetsObject :: IO FilePath
-- getDataFileName'RIXSetsObject = getDataFileName fp'RIXSetsObject

----------------------------------------

----------------------------------------
