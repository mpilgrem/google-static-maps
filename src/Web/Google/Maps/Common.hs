{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Web.Google.Maps.Common
-- Description : Bindings to the Google Static Maps API
-- Copyright   : (c) Mike Pilgrem 2017
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
-- 
-- This module has no connection with Google Inc. or its affiliates.
module Web.Google.Maps.Common
    ( -- * Functions
      googleMapsApis
        -- * Types
    , Key      (..)
    , Location (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Double.Conversion.Text (toFixed)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T (concat)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (..))
import Servant.Client (BaseUrl (..), Scheme (..))

-- | API key
newtype Key = Key Text
    deriving (Eq, Show, ToHttpApiData)

-- | Location: precision in latitude or longitude beyond 6 decimal places is
-- ignored.
data Location = Location
    { lat :: Double  -- ^ Takes any value between -90 and 90.
    , lng :: Double  -- ^ Takes any value between -180 and 180.
    } deriving (Eq, Show, Generic)

instance ToHttpApiData Location where
    toUrlPiece (Location lat' lng') = T.concat [toFixed precision lat', ",",
        toFixed precision lng']
      where
        precision = 6  -- Precision beyond 6 decimal places is ignored.

instance ToHttpApiData [Location] where
    toUrlPiece [] = ""
    toUrlPiece ls = T.concat $ intersperse pipe $ map toUrlPiece ls
      where
        pipe = toUrlPiece ("|" :: Text)

instance FromJSON Location

-- | The base URL for the Google Maps APIs.
googleMapsApis :: BaseUrl
googleMapsApis = BaseUrl Https "maps.googleapis.com" 443 "/maps/api"
