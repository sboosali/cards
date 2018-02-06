{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PackageImports #-}

{-|


write custom unicode-show, it's too slow on the large mtg json files. 

use CPP and unicode-show to inline ByteStrings without
TemplateHaskell. 

inline and optimize @unicode-show@: <https://hackage.haskell.org/package/unicode-show-0.1.0.2/docs/src/Text-Show-Unicode.html#ushow>

-}
module Cards.Common.UnicodeShow where

import           Data.Text                    (Text)
import qualified Data.Text.Lazy       as Lazy (Text)

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

import "unicode-show" Text.Show.Unicode (ushowWith)

import Data.Char

import Prelude.Spiros hiding (Text,ByteString)

----------------------------------------

ushow :: Show a => a -> String
ushow = ushowWith
 (\c -> isPrint c && not (isHaskellEscaped c))
 where
 isHaskellEscaped c
   = c == '\\' 
  || c == '\''
  || c == '\"'

{-# INLINE ushow #-}
{-# SPECIALIZE ushow :: Text            -> String #-}
{-# SPECIALIZE ushow :: Lazy.Text       -> String #-}
{-# SPECIALIZE ushow :: ByteString      -> String #-}
{-# SPECIALIZE ushow :: Lazy.ByteString -> String #-}

----------------------------------------

-- | Parse one Haskell character literal expression from a 'String' produced by 'show', and
--
--  * If the found char satisfies the predicate, replace the literal string with the character itself.
--  * Otherwise, leave the string as it was.
--  * Note that special delimiter sequence "\&" may appear in a string. c.f.  <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6 Section 2.6 of the Haskell 2010 specification>.
--
-- > Consistent with the “maximal munch” rule, numeric escape characters in strings consist of all consecutive digits and may be of arbitrary length. Similarly, the one ambiguous ASCII escape code, "\SOH", is parsed as a string of length 1. The escape character \& is provided as a “null character” to allow strings such as "\137\&9" and "\SO\&H" to be constructed (both of length two). Thus "\&" is equivalent to "" and the character '\&' is disallowed. Further equivalences of characters are defined in Section 6.1.2.
--

----------------------------------------

