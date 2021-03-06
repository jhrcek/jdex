module Jdex.Parse (
    -- Extracting info from javadoc files
      directKnownSubclasses
    , getIndexLinks
    -- File <-> Class name conversoins
    , fqcnToJdFile
    , jdFileToFQCN
    , jdFileToSCN
    , inheritancePath
    ) where

import Control.Arrow ((&&&), (>>>))
import Control.Arrow.ArrowList ((>>.))
import Control.Arrow.ArrowTree ((//>))
import Data.Char (isAlpha, isUpper, toUpper)
import Data.List (isPrefixOf, intercalate)
import System.FilePath ((</>), dropExtension, takeBaseName)
import Text.HandsomeSoup (css)
import Text.XML.HXT.Core (readDocument, withParseHTML, withWarnings, runX, ArrowXml, XmlTree, getAttrValue, IOStateArrow, getText)
import Text.XML.HXT.XPath.Arrows (getXPathTrees)
import Text.XML.HXT.Arrow.XmlState.TypeDefs (IOSArrow)

import Jdex.Types

-- TODO - only valid for classes
directKnownSubclasses :: FilePath -> IO [Link]
directKnownSubclasses javadocFile = extractInfo javadocFile subclassesArrow
    where subclassesArrow = getXPathTrees "//h2/following-sibling::dl/dt/b[contains(text(),'Direct Known Subclasses')]/../../dd" >>> getLinks

inheritancePath :: FilePath -> IO [String] --TODO : Use FQN instead of String
inheritancePath javadocFile = do
    messyInheritanceLines <- extractInfo javadocFile parentClassesUpToObjectArrow
    return . map discardGenericParams . textsToClassFQNs $ messyInheritanceLines 
  where 
      parentClassesUpToObjectArrow = getXPathTrees "//h2/following-sibling::pre[1]" //> getText  -- >>. ({-filter (not.null) . map discardGenericParams .-} textsToClassFQNs)
      -- Helper to normalize messy stuff from getText
      -- in goes: ["\njava.lang.Object\n  "
      --          ,"com.google.web.bindery.event.shared.Event","<H>\n      "
      --          ,"com.google.gwt.event.shared.GwtEvent","<","BeforeSelectionHandler","<T>>\n          "
      --          ,"com.google.gwt.event.logical.shared.BeforeSelectionEvent<T>","\n"]
      -- out comes: ["java.lang.Object"
      --            ,"com.google.web.bindery.event.shared.Event<H>"
      --            ,"com.google.gwt.event.shared.GwtEvent<BeforeSelectionHandler<T>>"
      --            ,"com.google.gwt.event.logical.shared.BeforeSelectionEvent<T>"]
      textsToClassFQNs :: [String] -> [String]
      textsToClassFQNs = tail {-1st line always empty -} . lines . filter (/=' ') . concat

      discardGenericParams :: String -> String --TODO FQN instead of String?
      discardGenericParams = takeWhile (/='<')

getIndexLinks :: FilePath -> IO [Link]
getIndexLinks javadocRootDir = extractInfo (getIndexFile javadocRootDir) getLinks


-- | relative javadoc html FilePath to fully qualified class name (e.g. "java/lang/String.html" -> "java.lang.String")
jdFileToFQCN :: FilePath -> String
jdFileToFQCN = replace '/' '.' . dropStartingDots . dropExtension
    where dropStartingDots = dropWhile (not . isAlpha)


-- | Convert fully qualified class name to its corresponding javadoc html file
-- TODO improve performance - can't just replace all '.' with '/' because of nested classes like org.my.Outer.Inner -> org/my/Outer.Inner.html
fqcnToJdFile :: String -> FilePath
fqcnToJdFile = (++ ".html") . addPathSlashes . splitPkgCls
    where addPathSlashes (pkgs, clss) = intercalate "/" pkgs ++ "/" ++ intercalate "." clss
          splitPkgCls = break (isUpper . head) . words . replace '.' ' '


-- | Javadoc file path (java/lang/String.html) to simple ClassName, like "String"
jdFileToSCN :: FilePath -> String
jdFileToSCN = takeBaseName



----- Private implementation -----

-- Arrow to extract class/interface/enum/annotation info from javadoc index file's "a" elements:
-- e.g: <a title="class in com.google.gwt.core.ext" href="path/to/javadoc/file.html" ... will be mapped to ("class", "path/to/javadoc/file.html", True)
getLinks :: ArrowXml a =>  a XmlTree Link
getLinks = css "a" >>> (getAttrValue "title" &&& getAttrValue "href") >>. map parseTitleAndHref
  where
    parseTitleAndHref :: (String, String) -> Link
    parseTitleAndHref (title, href) = Link (extractConstruct title) href (not $ "http" `isPrefixOf` href)

    extractConstruct :: String -> Construct
    extractConstruct title = read . (\x -> toUpper (head x) : tail x) $ takeWhile (/=' ') title

-- Given Javadoc file path and an arrow for extracting items from the file, return IO action returning list of those items
extractInfo :: FilePath -> IOSArrow XmlTree a -> IO [a]
extractInfo file arrow = let parsedFile = parseHtmlFile file in runX $ parsedFile >>> arrow


getIndexFile :: FilePath -> FilePath
getIndexFile rootJavadocDir = rootJavadocDir </> "allclasses-noframe.html"


parseHtmlFile :: FilePath -> IOStateArrow s b XmlTree
parseHtmlFile = readDocument [withParseHTML True, withWarnings False]

replace :: Char -> Char -> String -> String
replace x y = map (\z -> if z == x then y else z)
