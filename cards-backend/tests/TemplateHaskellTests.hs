{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Lens

-- NOTE below we mock out any TemplateHaskell tests with `lens`'s
-- if your package doesn't provide TemplateHaskell-macros,
-- remove this file and the stanza

data Bar a b c = Bar { _baz :: (a, b) }
makeLenses ''Bar

checkBaz :: Iso (Bar a b c) (Bar a' b' c') (a, b) (a', b')
checkBaz = baz

main :: IO ()
main = putStrLn "[tests/TemplateHaskellTests.hs] ok"

