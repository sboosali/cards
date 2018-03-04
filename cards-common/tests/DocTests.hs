{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest       (doctest)
import System.Environment (getArgs)

----------------------------------------

main = do
  arguments <- getArgs
  
  doctest $ sources ++ extensions ++ arguments

----------------------------------------

sources =
 [ "sources/" 
 ] 

extensions = extensions2flags "NoImplicitPrelude PackageImports AutoDeriveTypeable DeriveDataTypeable DeriveGeneric DeriveFunctor DeriveFoldable DeriveTraversable LambdaCase EmptyCase TypeOperators PostfixOperators ViewPatterns BangPatterns KindSignatures NamedFieldPuns RecordWildCards TupleSections MultiWayIf DoAndIfThenElse EmptyDataDecls MultiParamTypeClasses FlexibleContexts FlexibleInstances TypeFamilies FunctionalDependencies ScopedTypeVariables StandaloneDeriving"
  where
  extensions2flags :: String -> [String]
  extensions2flags = fmap ("-X"++) . words

----------------------------------------

{-

There's two sets of GHC extensions involved when running Doctest:

- The set of GHC extensions that are active when compiling the module code (excluding the doctest examples). The easiest way to specify these extensions is through LANGUAGE pragmas in your source files. (Doctest will not look at your cabal file.)

- The set of GHC extensions that are active when executing the Doctest examples. (These are not influenced by the LANGUAGE pragmas in the file.) The recommended way to enable extensions for Doctest examples is to switch them on like this:

    -- |
    -- >>> :set -XTupleSections
    -- >>> fst' $ (1,) 2
    -- 1
    fst' :: (a, b) -> a
    fst' = fst

Alternatively you can pass any GHC options to Doctest, e.g.:

    doctest -XCPP Foo.hs

-}
