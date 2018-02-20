module Cards.Common.Core where
import Cards.Common.Extra
-- import Cards.Common.Types

import qualified Data.Text as T

----------------------------------------

normalize :: Text -> Text 
normalize = T.toCaseFold >>> T.words >>> T.unwords

----------------------------------------
