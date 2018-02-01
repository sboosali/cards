{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The core definitions. 

-}
module Cards.Frontend.Core where

import Cards.Frontend.Extra
--import Cards.Frontend.Types
--import Cards.Frontend.DB

--import Reflex hiding (Query)
--import qualified Reflex as R
import Reflex.Dom hiding (Query)

--import qualified Control.Lens as L

--import qualified Data.Map as Map
-- import Data.Map (Map)

-- import qualified Data.Text as T
-- import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

{-NOTES

create an HTML element, with (1) Dynamic attributes and a (2) child element, returning its Events:

    elDynAttr'
     :: Text
     -> Dynamic (Map Text Text)
     -> m a 
     -> m (El, a)

elDynAttr' ~ elDynAttr' NoXMLNamespace :

    elDynAttr'
     = elDynAttrNS' Nothing

elDynAttr ~ snd <&> elDynAttr' :

    elDynAttr tag dAttributes child
     = snd <$> elDynAttr' tag dAttributes child

elDynClass ~ ("class" =:) <$> elDynAttr :

    elDynClass tag dClass
     = elDynAttr tag (("class" =:) <$> dClass)

elAttr ~ pure >>> elDynAttr :

    elAttr tag attributes child
     = elDynAttr tag (pure attributes) child
    

type El = Element EventResult GhcjsDomSpace



module Reflex.Dom.Widget.Basic

button :: DomBuilder t m => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  return $ domEvent Click e

i.e.

button :: (... t m) => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  let eClick = domEvent Click e 
  return eClick

button' :: MonadWidget t m => Text -> m ( t ())
button' t = do
  (esButton, _) <- element "button" def $ text t
  -- let eClick = domEvent Click esButton
  return esButton



-}

----------------------------------------

{-|

-}
checkboxBoundedEnum :: (Enum a, Bounded a) => AttributeMap -> [a]
checkboxBoundedEnum _ = constructors

----------------------------------------
