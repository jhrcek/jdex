{- | Tool to extract information about Java constructs (i.e. Classes, Interface,
 Annotations and Enums) from Javadoc HTML files stored in local filesystem.
-}
module Jdex.Jdex where

import Control.Arrow ((&&&))
import Data.List (sort, group)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Jdex.Types
import Jdex.Parse

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "usage: jdex <path-to-javadoc-root-dir>"
        (rootJavadocDir:_) -> processJavadoc rootJavadocDir

processJavadoc :: FilePath -> IO ()
processJavadoc rootJdDir = printSubclassTree rootJdDir "com.google.gwt.user.client.ui.UIObject"

printSubclassTree :: FilePath -> String -> IO ()
printSubclassTree jdroot fcqn = do
    let startFile = jdroot </> fqcnToJdFile fcqn
    subclassLinks <- directKnownSubclasses startFile
    --print all subclass links
    mapM_ (\sublink -> putStrLn $ show (simpleClassName sublink) ++ " -> " ++ show (jdFileToSCN startFile)) subclassLinks
    -- continue recursively for each subclass
    mapM_ (printSubclassTree jdroot . jdFileToFQCN . lFile) subclassLinks

simpleClassName :: Link -> String
simpleClassName = jdFileToSCN . lFile

-- | How many pieces of each construct (class, interface, annotation, enum) does the list contain?
linksSummary :: [Link] -> [(Construct, Int)]
linksSummary = map (head &&& length) . group . sort . map lConstruct
