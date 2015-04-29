{- | Tool to extract information about Java constructs (i.e. Classes, Interface,
 Annotations and Enums) from Javadoc HTML files stored in local filesystem.
-}
module Jdex.Jdex where

import Control.Arrow ((&&&))
import Data.List (sort, group)
import Data.Tree (Tree(..), unfoldTreeM_BF, rootLabel)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Jdex.Types
import Jdex.Parse

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "usage: jdex <path-to-javadoc-root-dir>"
        (rootJavadocDir:_) -> printSubclassHierarchyAsDot rootJavadocDir "com.google.gwt.user.server.rpc.AbstractRemoteServiceServlet"

printSubclassHierarchyAsDot :: FilePath -> String -> IO ()
printSubclassHierarchyAsDot rootJdDir fqcn = do
    subclassTree <- extractSubclassTree rootJdDir fqcn
    putStrLn "digraph {\nrankdir=BT;\noverlap=false;\nedge[arrowhead=empty];\n"
    putStrLn . unlines $ treeToGraphvizEdges subclassTree
    putStrLn "}"

extractSubclassTree :: FilePath -> String -> IO (Tree String)
extractSubclassTree jdroot fqcn = unfoldTreeM_BF (myUnfolder jdroot) fqcn
  where
    -- given root javadoc dir + fqcn, return (fqcn, [fqcn's of direct known subclasses])
    myUnfolder :: FilePath -> String -> IO (String, [FilePath])
    myUnfolder root fqcn' = do
        subclassLinks <- directKnownSubclasses $ root </> fqcnToJdFile fqcn'
        return (fqcn', map (jdFileToFQCN . lFile) subclassLinks)

-- | How many pieces of each construct (class, interface, annotation, enum) does the list contain?
linksSummary :: [Link] -> [(Construct, Int)]
linksSummary = map (head &&& length) . group . sort . map lConstruct


-- Tree to list like: ["a -> b", "a -> c", "b -> c", ...]
treeToGraphvizEdges :: Show a => Tree a -> [String]
treeToGraphvizEdges (Node node subtrees) = map ((\child -> show node ++ " -> " ++ show child) . rootLabel) subtrees ++ concatMap treeToGraphvizEdges subtrees

