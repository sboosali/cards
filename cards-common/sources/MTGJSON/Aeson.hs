{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}

{-| 

-}
module MTGJSON.Aeson where

import Data.Aeson (defaultOptions, genericParseJSON) 
import Data.Aeson.Types (Options(..)) 
import qualified Data.Aeson as J 
import qualified Data.Aeson.Types  as J

import Data.ByteString.Lazy (ByteString) 

import qualified Data.Text as T
import Data.Text() 

import GHC.Generics  
import GHC.TypeLits(KnownSymbol, symbolVal) 
import Control.Monad.Fail (MonadFail)

import Prelude.Spiros 

----------------------------------------

decoded  :: (MonadFail m, J.FromJSON a) => ByteString -> m a
decoded  = J.eitherDecode > either fail return 

----------------------------------------

-- concatenateA :: (Applicative f, Traversable t) => (a -> f [b]) -> t a -> f [b]
concatenateA :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatenateA f = traverse f >>> fmap concat -- join 

----------------------------------------

{-| constrain a type @a@ to be datatype (@data@ or @newtype@). 

only @a@ and @name@ are relevant. 

-}
type IsDataType a name m p t f
 = ( Generic a
   , KnownSymbol name
   , (Rep a) ~ (M1 D ('MetaData name m p t) f)
   ) 

{-| my naminimport g convention for types that are json schemas.

usage: 

@
instance 'FromJSON' where 'parseJSON' = 'genericParseJSON' myOptions 
@ 

strips the typename and leading underscores from field names. for example, given

    data CardType = CardType { _CardType_theField :: ... } deriving ('Generic') 

the original Haskell record field name 

    "_CardType_theField" 

becomes this json object field name 

    "theField" 

via 'stripTypePrefixFromFieldLabel'. 

-}
parseJSON_TypePrefix 
  :: forall a name. forall m p t f. 
   ( J.GFromJSON J.Zero (Rep a)
   , IsDataType a name m p t f
   )
  => J.Value -> J.Parser a
parseJSON_TypePrefix = genericParseJSON o
  where
  o = (options_StripTypePrefix (Proxy :: Proxy a))
      { omitNothingFields = True
      }

  -- o  = options_StripTypePrefix (Proxy :: Proxy a)
  --   <> options_MissingMaybes

options_StripTypePrefix
  :: forall a name proxy. forall m p t f. 
   ( IsDataType a name m p t f) 
  => proxy a 
  -> Options 
options_StripTypePrefix proxy = defaultOptions
  { fieldLabelModifier = stripTypePrefixFromFieldLabel proxy  
  }

-- options_MissingMaybes :: Options 
-- options_MissingMaybes = defaultOptions
--   { omitNothingFields  = True
--   }

{-| 

>>> :set -XDeriveGeneric 
>>> data CardType = CardType { _CardType_theField :: ... } deriving (Generic) 
>>> stripTypePrefixFromFieldLabel ([] :: [CardType]) "_CardType_theField"
"theField"  

-}
stripTypePrefixFromFieldLabel 
  :: forall a name proxy. forall m p t f.
     ( IsDataType a name m p t f ) 
  => proxy a 
  -> String -> String 
stripTypePrefixFromFieldLabel proxy = T.pack > go > T.unpack
  where
  prefix              = T.pack ("_" <> dataTypeNameOf proxy <> "_")
  go     oldFieldName = T.stripPrefix prefix oldFieldName & \case 
    Just newFieldName -> newFieldName 
    Nothing           -> oldFieldName -- TODO error or fall back?

--   :: (Generic a, M1 D meta ~ (Rep a), Datatype meta) 
{- 
  :: ( Generic a
     , (Rep a) ~ (M1 D meta) i
     , meta    ~ (MetaData name m p nt)
     , Datatype meta
     ) 
  => proxy a 
  -> String -> String 

  where
  name                = datatypeName (Proxy :: Proxy meta)
-}

{-| 

>>> :set -XDeriveGeneric 
>>> data CardType = CardType { _CardType_theField :: ... } deriving (Generic) 
>>> dataTypeNameOf ([] :: [CardType])
"CardType"  

-}
dataTypeNameOf
  :: forall a name proxy. forall m p t f.
   ( IsDataType a name m p t f ) 
  => proxy a 
  -> String 
dataTypeNameOf _ = symbolVal (Proxy :: Proxy name) 

----------------------------------------
