
{-|

-}
module MTGJSON.Kinds where

-- import Prelude.Spiros

----------------------------------------

data CHARACTERISTIC
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
 
----------------------------------------

{-

data KIND
 = SUPERTYPE
 | TYPE
 | SUBTYPE

-}

---------------------------------------
