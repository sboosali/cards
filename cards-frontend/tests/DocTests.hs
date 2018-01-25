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

 -- [ "sources/Cards/Frontend/Extra.hs"
 -- , "sources/Cards/Frontend/Types.hs"
 -- , "sources/Cards/Frontend/Core.hs"
 -- , "sources/Cards/Frontend.hs"
 -- , "sources/Cards/Frontend/Example.hs"
 -- ]

