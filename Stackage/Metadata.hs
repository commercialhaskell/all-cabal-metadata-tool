{-# LANGUAGE OverloadedStrings #-}
module Stackage.Metadata
    ( PackageInfo (..)
    , Deprecation (..)
    ) where

import Data.Version (Version)
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Aeson
import Stackage.PackageIndex.Conduit (renderDistText, parseDistText)

data PackageInfo = PackageInfo
    { piLatest :: !Version
    , piHash :: !Text
    , piAllVersions :: !(Set Version)
    , piSynopsis :: !Text
    , piDescription :: !Text
    , piDescriptionType :: !Text
    , piChangeLog :: !Text
    , piChangeLogType :: !Text
    }
    deriving (Show, Eq, Typeable)
instance ToJSON PackageInfo where
    toJSON pi = object
        [ "latest" .= renderDistText (piLatest pi)
        , "hash" .= piHash pi
        , "all-versions" .= map renderDistText (Set.toList $ piAllVersions pi)
        , "synopsis" .= piSynopsis pi
        , "description" .= piDescription pi
        , "description-type" .= piDescriptionType pi
        , "changelog" .= piChangeLog pi
        , "changelog-type" .= piChangeLogType pi
        ]
instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \o -> PackageInfo
        <$> (o .: "latest" >>= parseDistText)
        <*> o .: "hash"
        <*> (o .: "all-versions" >>= fmap Set.fromList . mapM parseDistText)
        <*> o .: "synopsis"
        <*> o .: "description"
        <*> o .: "description-type"
        <*> o .: "changelog"
        <*> o .: "changelog-type"

data Deprecation = Deprecation
    { depPackage :: !Text
    , depInFavourOf :: !(Set Text)
    }
instance ToJSON Deprecation where
    toJSON d = object
        [ "deprecated-package" .= depPackage d
        , "in-favour-of" .= depInFavourOf d
        ]
instance FromJSON Deprecation where
    parseJSON = withObject "Deprecation" $ \o -> Deprecation
        <$> o .: "deprecated-package"
        <*> o .: "in-favour-of"
