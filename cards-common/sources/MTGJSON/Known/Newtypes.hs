
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

NOTE: Like "MTGJSON.Known.Types@, which uses @DeriveAnyClass@, but works around @GHC 8.0@'s lack of @-XDerivingStrategies@, where this module uses @GeneralizedNewtypeDeriving@. 

-}
module MTGJSON.Known.Newtypes where

import MTGJSON.Extra
import MTGJSON.Known.Types

import Enumerate

import Control.Lens hiding ((<&>))

import qualified Data.Set as Set

import GHC.Exts (IsList(..))

----------------------------------------
  
type KnownColors = Colors
  
newtype Colors = Colors
  (Set Color)
  deriving (Show,Read,Eq,Ord,NFData,Generic)
  -- deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

instance Wrapped Colors where

instance Enumerable Colors where

  enumerated = coerceColorIdentities enumerated
    where
    coerceColorIdentities
      :: [Unwrapped Colors]
      -> [          Colors] 
    coerceColorIdentities = coerce

  cardinality _ = cardinality (P :: P (Unwrapped Colors))
  
instance IsList (Colors) where
 type Item (Colors) = Color
 toList   = getColors >>> Set.toList
 fromList = toColors

getColors :: Colors -> Set Color
getColors (Colors colors) = colors

toColors :: [Color] -> Colors
toColors = Set.fromList >>> Colors

colorless :: Colors
colorless = Colors Set.empty

----------------------------------------

-- data ColorIdentity
--  = ColorIdentity Chroma
--  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

-- data ColorIndication
--  = ColorIndication Color
--  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)


{-|

@
> 903.4. The Commander variant uses color identity to determine what cards can be in a deck with a certain commander. The color identity of a card is the color or colors of any mana symbols in that card’s mana cost or rules text, plus any colors defined by its characteristic-defining abilities (see rule 604.3) or color indicator (see rule 204).

> there are multiple exceptions: cards with Deviod can't be used for one reason, Transguild Courier can't be used for another, and Elbrus, the Binding Blade isn't allowed for yet another reason.
> - murgatroid99 (stackexchange)

> https://boardgames.stackexchange.com/questions/31327/if-i-use-a-colorless-commander-can-i-use-colored-spells
@

-}
newtype ColorIdentity = ColorIdentity
  { _Color_identity :: Set Color
  }
  deriving (Show,Read,Eq,Ord,NFData,Generic)
  -- deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

instance Wrapped ColorIdentity where
  -- type Unwrapped ColorIdentity = Set Color

instance Enumerable ColorIdentity where

  enumerated = coerceColorIdentities enumerated
    where
    coerceColorIdentities
      :: [Unwrapped ColorIdentity]
      -> [          ColorIdentity] 
    coerceColorIdentities = coerce

  cardinality _ = cardinality (P :: P (Unwrapped ColorIdentity))

colorlessIdentity :: ColorIdentity
colorlessIdentity = ColorIdentity Set.empty

{-ERROR

    • Couldn't match representation of type ‘proxy (Set Color)’
                               with that of ‘proxy ColorIdentity’
        arising from the coercion of the method ‘Enumerate.Types.cardinality’
          from type ‘forall (proxy :: * -> *). proxy (Set Color) -> Natural’
            to type ‘forall (proxy :: * -> *). proxy ColorIdentity -> Natural’
      NB: We cannot know what roles the parameters to ‘proxy’ have;
        we must assume that the role is nominal
    • When deriving the instance for (Enumerable ColorIdentity)

-}

----------------------------------------

{-|

-}  
newtype ColorIndication = ColorIndication
  { _Color_indication :: Set Color
  }
  deriving (Show,Read,Eq,Ord,NFData,Generic)
  -- deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

instance Wrapped ColorIndication where

instance Enumerable ColorIndication where

  enumerated = coerceColorIdentities enumerated
    where
    coerceColorIdentities
      :: [Unwrapped ColorIndication]
      -> [          ColorIndication] 
    coerceColorIdentities = coerce

  cardinality _ = cardinality (P :: P (Unwrapped ColorIndication))

colorlessIndication :: ColorIndication
colorlessIndication = ColorIndication Set.empty

-- type ColorIndication = [ColorIndicator]

----------------------------------------
