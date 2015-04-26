module Types (
    Construct (..)
  , isClass, isAnnotation, isInterface, isEnum
  , Link(..)
  ) where

-- | Type of java constructs that can be documented in Javadoc file
data Construct = Class | Interface | Annotation | Enum deriving (Show, Read, Eq, Ord)

-- | Info extractable from javadoc html link, that points to a Java construct javadoc html file.
data Link = Link
    { lConstruct :: Construct -- | The construct this link points to
    , lFile :: FilePath       -- | File path relative to javadoc root directory this link points to
    , lIsLocal :: Bool        -- | True if this links shows path in local filesystem, False otherwise (i.e. points to some http://)
    } deriving (Show, Eq, Ord)

isClass, isInterface, isAnnotation, isEnum :: Link -> Bool
isClass      = (Class ==)      . lConstruct
isInterface  = (Interface ==)  . lConstruct
isAnnotation = (Annotation ==) . lConstruct
isEnum       = (Enum ==)       . lConstruct
