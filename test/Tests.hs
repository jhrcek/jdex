module Main where

import Control.Monad (void)
import Test.HUnit ((~?=), runTestTT, Test(TestList))

import Jdex.Parse


main :: IO ()
main = void . runTestTT $ TestList
    [ jdFileToFQCN "org/graphstream/ui/swingViewer/LayerRenderer.html" ~?= "org.graphstream.ui.swingViewer.LayerRenderer"
    , jdFileToFQCN "com/google/gwt/user/datepicker/client/CellGridImpl.Cell.html" ~?= "com.google.gwt.user.datepicker.client.CellGridImpl.Cell"

    , jdFileToSCN "org/graphstream/ui/swingViewer/LayerRenderer.html" ~?= "LayerRenderer"
    , jdFileToSCN "com/google/gwt/user/datepicker/client/CellGridImpl.Cell.html" ~?= "CellGridImpl.Cell"

    , fqcnToJdFile "java.lang.String" ~?= "java/lang/String.html"
    , fqcnToJdFile "com.google.gwt.user.datepicker.client.CellGridImpl.Cell" ~?= "com/google/gwt/user/datepicker/client/CellGridImpl.Cell.html"
    , fqcnToJdFile "org.graphstream.ui.swingViewer.LayerRenderer" ~?= "org/graphstream/ui/swingViewer/LayerRenderer.html"
    ]
