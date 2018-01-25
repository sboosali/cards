{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest

main :: IO ()
main = doctest

   -- every module in this directory (i.e. `hs-source-dirs`)
 [ "sources/"

   -- all language extensions enabled by default (i.e. `default-extensions`)
 , "-XPackageImports"
 , "-XAutoDeriveTypeable"
 , "-XDeriveDataTypeable"
 , "-XDeriveGeneric"
 , "-XDeriveFunctor"
 , "-XDeriveFoldable"
 , "-XDeriveTraversable"
 , "-XLambdaCase"
 , "-XEmptyCase"
 , "-XTypeOperators"
 , "-XPostfixOperators"
 , "-XViewPatterns"
 , "-XBangPatterns"
 , "-XKindSignatures"
 , "-XNamedFieldPuns"
 , "-XRecordWildCards"
 , "-XTupleSections"
 , "-XMultiWayIf"
 , "-XDoAndIfThenElse"
 , "-XEmptyDataDecls"
 , "-XMultiParamTypeClasses"
 , "-XFlexibleContexts"
 , "-XFlexibleInstances"
 , "-XTypeFamilies"
 , "-XFunctionalDependencies"
 , "-XScopedTypeVariables"
 , "-XStandaloneDeriving"
 ]

 -- [ "sources/Cards/Backend/Extra.hs"
 -- , "sources/Cards/Backend/Types.hs"
 -- , "sources/Cards/Backend/Core.hs"
 -- , "sources/Cards/Backend.hs"
 -- , "sources/Cards/Backend/Example.hs"
 -- ]

