{- | Tool to extract information about Java constructs (i.e. Classes, Interface,
 Annotations and Enums) from Javadoc HTML files stored in local filesystem.
-}
module Jdex.Jdex where

import Control.Arrow ((&&&))
import Data.List (sort, group, foldl')
import Data.Tree (Tree(..), Forest, unfoldTreeM_BF, rootLabel)
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

-- | Given javadoc root dir and fqn of a class return inheritance tree rooted at that class
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


classInheritanceHierarchy jdRoot = do
    indexLinks <- getIndexLinks jdRoot
    let allClassJdFiles = map ((jdRoot </>) . lFile) . filter isClass $ indexLinks
    allClassInheritancePaths <- mapM (\jdfile -> do print jdfile ; inheritancePath jdfile) allClassJdFiles
    return allClassInheritancePaths --TODO after removing generic param info from inheritancePath, build tree using pathListToTree

-- | Tree manipulation
-- Tree to list like: ["a -> b", "a -> c", "b -> c", ...]
treeToGraphvizEdges :: Show a => Tree a -> [String]
treeToGraphvizEdges (Node root subtrees) = map (parentChildEdge root . rootLabel) subtrees ++ concatMap treeToGraphvizEdges subtrees
   where parentChildEdge parent child = show parent ++ " -> " ++ show child

-- Transform list of paths (like [[a,b,c],[a,b,d],..]) into tree like
-- a - b - c
--      \- d
-- The idea is to use this to build class inheritance tree from inheritance paths obtained from class's javadoc files
pathListToTree :: Eq a => [[a]] -> Forest a
pathListToTree = foldl' addPath []
  where
    addPath :: Eq b => Forest b -> [b] -> Forest b
    addPath forest [] = forest
    addPath [] (x:xs) = [Node x (addPath [] xs)]
    addPath (t@(Node root subforest):ts) (x:xs)
       | root == x  = Node root (addPath subforest xs) : ts
       | otherwise  = t : addPath ts (x:xs)

