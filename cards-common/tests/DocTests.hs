{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest

main = doctest
 [ "sources/Cards/Common/Extra.hs"
 , "sources/Cards/Common/Types.hs"
 , "sources/Cards/Common/Core.hs"
 , "sources/Cards/Common.hs"
 , "sources/Cards/Common/Example.hs"
 ]
