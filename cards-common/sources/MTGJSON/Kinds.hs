
{-|

-}
module MTGJSON.Kinds where

-- import Prelude.Spiros

----------------------------------------

data CHARACTERISTIC

 = CARD            -- ^ 

 | UNIQUE          -- ^ 
 | NAME            -- ^
 | MULTIVERSEID    -- ^

 | COLOR           -- ^ 
 | CHROMA          -- ^ 

 | MANA            -- ^ 
 | COST            -- ^ 

 | NUMERIC         -- ^ 

 | SUPERTYPE       -- ^ 
 | TYPE            -- ^ 
 | SUBTYPE         -- ^ 

 | ORACLE          -- ^ 
  
 | FACE            -- ^
 {- TODO | LAYOUT    -}
 
 | RARITY          -- ^
 | WATERMARK       -- ^ 

 | EDITION         -- ^
 | BLOCK           -- ^
 
 | FORMAT          -- ^ 
 | LEGALITY        -- ^ 

 | LANGUAGE        -- ^ 

 | ASSETS          -- ^ 
 {- TODO | IMAGE    -}
 


{-
 = UNIQUE          -- ^ 
 | NAME            -- ^ 
 | MANACOST        -- ^ 
 | COLOR           -- ^ 

 | ORACLE          -- ^ 
 | NUMBER          -- ^ 
 | CMC             -- ^ 
 | COLORIDENTITY   -- ^ 
  
 | SUPERTYPE       -- ^ 
 | TYPE            -- ^ 
 | SUBTYPE         -- ^ 
  
 | LAYOUT          -- ^ 
 | WATERMARK       -- ^ 
 | RARITY          -- ^
 | EDITIOn         -- ^
 | FLAVOR          -- ^ 
 | ARTIST          -- ^ 
 | COLLECTORNUMBER -- ^ 
 | MULTIVERSEID    -- ^ 

 | RULING          -- ^ 
 | LEGALITY        -- ^ 

 | VARIATION       -- ^ 
 | PRINTING        -- ^ 
 | FOREIGNVARIATION -- ^ 

 | LANGUAGE        -- ^ 
 | FORMAT          -- ^ 

 | TEXT            -- ^
 | DATE            -- ^
-}

----------------------------------------

{-

data KIND
 = SUPERTYPE
 | TYPE
 | SUBTYPE

-}

---------------------------------------
