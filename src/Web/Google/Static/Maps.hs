{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Web.Google.Static.Maps
-- Description : Bindings to the Google Static Maps API
-- Copyright   : (c) Mike Pilgrem 2017
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
-- 
-- This module has no connection with Google Inc. or its affiliates.
-- 
-- The <https://developers.google.com/maps/documentation/static-maps/intro Google Static Maps API>
-- returns a map as an image via an HTTP request. This library provides bindings
-- in Haskell to that API (version 2).
--
-- NB: The use of the Google Static Maps API services is subject to the
-- <https://developers.google.com/maps/terms Google Maps APIs Terms of Service>,
-- which terms restrict the use of content.
--
-- The following are not yet implemented: certain optional parameters
-- ('language', and 'region'); address locations; non-PNG image
-- formats; and encoded polyline paths.
--
-- The code below is an example console application to test the use of the
-- library with the Google Static Maps API.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > module Main (main) where
-- > 
-- > import Codec.Picture.Saving (imageToPng)               -- package JuicyPixels
-- > import qualified Data.ByteString.Lazy as B (writeFile)
-- > import Data.Maybe (fromJust)
-- > import Graphics.Gloss (Display (..), display, white)   -- package gloss
-- > import Graphics.Gloss.Juicy (fromDynamicImage)         -- package gloss-juicy
-- > import Network.HTTP.Client (newManager)
-- > import Network.HTTP.Client.TLS (tlsManagerSettings)
-- > import Web.Google.Static.Maps (Center (..), Key (..), Location (..), Size (..),
-- >     staticmap, StaticmapResponse (..), Zoom (..))
-- > 
-- > main :: IO ()
-- > main = do
-- >     putStrLn "A test of the Google Static Maps API.\nNB: The use of the \
-- >         \API services is subject to the Google Maps APIs Terms of Serivce \
-- >         \at https://developers.google.com/maps/terms.\n"
-- >     mgr <- newManager tlsManagerSettings
-- >     let apiKey = Key "<REPLACE_THIS_WITH_YOUR_ACTUAL_GOOGLE_API_KEY>"
-- >         center = Just $ Center (Location 42.165950 (-71.362015))
-- >         zoom   = Just $ Zoom 17
-- >         w      = 400
-- >         h      = 400
-- >         size   = Size w h
-- >     result <- staticmap mgr apiKey Nothing center zoom size Nothing Nothing
-- >                   [] Nothing [] [] Nothing
-- >     case result of
-- >         Right response -> do
-- >             let picture = fromJust $ fromDynamicImage response
-- >                 title   = "Test Google Static Maps API"
-- >                 window  = InWindow title (w, h) (10, 10)
-- >             display window white picture 
-- >         Left err -> putStrLn $ "Error! Result:\n" ++ show err
module Web.Google.Static.Maps
       ( -- * Functions
         staticmap
         -- * API
       , GoogleStaticMapsAPI
       , api
         -- * Types
       , Key               (..)
       , Signature         (..)
       , Center            (..)
       , Location          (..)
       , Zoom              (..)
       , Size              (..)
       , Scale             (..)
       , Format            (..)
       , MapType           (..)
       , MapStyle          (..)
       , Feature           (..)
       , Element           (..)
       , MapStyleOp        (..)
       , Visibility        (..)
       , Markers           (..)
       , MarkerStyle       (..)
       , MarkerSize        (..)
       , MarkerColor       (..)
       , MarkerLabel       (..)
       , StdColor          (..)
       , URI               (..)
       , URIAuth           (..)
       , Anchor            (..)
       , StdAnchor         (..)
       , Path              (..)
       , PathStyle         (..)
       , PathWeight        (..)
       , PathColor         (..)
       , PathGeodesic      (..)
       , Visible           (..)
       , StaticmapResponse
       ) where

import Codec.Picture.Types (DynamicImage (..))
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T (append, concat, pack)
import Data.Word (Word8)
import Network.HTTP.Client (Manager)
import Network.URI (URI (..), URIAuth (..), uriToString)
import Servant.API ((:>), Get, QueryParam, QueryParams, ToHttpApiData (..))
import Servant.Client (BaseUrl (..), client, ClientEnv (..), ClientM,
    runClientM, Scheme (..), ServantError)
import Servant.JuicyPixels (PNG)
import Text.Bytedump (hexString)

-- | API key
newtype Key = Key Text
    deriving (Eq, Show, ToHttpApiData)

-- | Signature
newtype Signature = Signature Text
    deriving (Eq, Show, ToHttpApiData)

-- | Center of the map: not required if the map includes markers or paths.
newtype Center = Center Location
    deriving (Eq, Show, ToHttpApiData)

-- | Location
data Location = Location
    { lat :: Double  -- ^ Takes any value between -90 and 90.
    , lng :: Double  -- ^ Takes any value between -180 and 180.
    } deriving (Eq, Show)

instance ToHttpApiData Location where
    toUrlPiece (Location lat' lng') = T.pack (show lat' ++ "," ++ show lng')

instance ToHttpApiData [Location] where
    toUrlPiece [] = ""
    toUrlPiece ls = T.concat $ intersperse pipe $ map toUrlPiece ls
      where
        pipe = toUrlPiece ("|" :: Text)

-- | Zoom level: the lowest level, in which the whole world can be seen, is 0.
-- Each succeeding level doubles the precision. Not required if the map includes
-- markers or paths.
newtype Zoom = Zoom Int
    deriving (Eq, Show, ToHttpApiData)

-- | Size in pixels: there are maximum allowable values.
data Size = Size
    { width  :: Int
    , height :: Int
    } deriving (Eq, Show)

instance ToHttpApiData Size where
    toUrlPiece (Size width' height') =
        T.pack (show width' ++ "x" ++ show height')

-- | Scale
data Scale
    = Single     -- ^ The default value.
    | Double
    | Quadruple
    deriving (Eq, Show)

instance ToHttpApiData Scale where
    toUrlPiece scale = case scale of
        Single    -> "1"
        Double    -> "2"
        Quadruple -> "4"

-- | Image format
data Format
    = Png8   -- ^ The default value.
    | Png32
    deriving (Eq, Show)

instance ToHttpApiData Format where
    toUrlPiece format = case format of
        Png8  -> "png8"
        Png32 -> "png32"

-- | Map type
data MapType
    = RoadMap    -- ^ The default value.
    | Satellite
    | Hybrid
    | Terrain
    deriving (Eq, Show)

instance ToHttpApiData MapType where
    toUrlPiece mapType = case mapType of
        RoadMap   -> "roadmap"
        Satellite -> "satellite"
        Hybrid    -> "hybrid"
        Terrain   -> "terrain"

-- | MapStyle
data MapStyle = MapStyle (Maybe Feature) (Maybe Element) [MapStyleOp]
    deriving (Eq, Show)

instance ToHttpApiData MapStyle where
    toUrlPiece (MapStyle featureOpt elementOpt ops) =
        T.concat $ intersperse pipe $ catMaybes [featureUrl, elementUrl] ++
            [opsUrl]
      where
        pipe = toUrlPiece ("|" :: Text)
        featureUrl = T.append "feature:" . toUrlPiece <$> featureOpt
        elementUrl = T.append "element:" . toUrlPiece <$> elementOpt
        opsUrl = toUrlPiece ops

-- | Map feature
data Feature
    = AllFeatures
    | Administrative
    | AdministrativeCountry
    | AdministrativeLandParcel
    | AdministrativeLocality
    | AdministrativeNeighborhood
    | AdministrativeProvince
    | Landscape
    | LandscapeManMade
    | LandscapeNatural
    | LandscapeNaturalLandcover
    | LandscapeNaturalTerrain
    | Poi
    | PoiAttraction
    | PoiBusiness
    | PoiGovernment
    | PoiMedical
    | PoiPark
    | PoiPlaceOfWorship
    | PoiSchool
    | PoiSportsComplex
    | Road
    | RoadArterial
    | RoadHighway
    | RoadHighwayControlledAccess
    | RoadLocal
    | Transit
    | TransitLine
    | TransitStation
    | TransitStationAirport
    | TransitStationBus
    | TransitStationRail
    | Water
    deriving (Eq, Show)

instance ToHttpApiData Feature where
    toUrlPiece feature = case feature of
        AllFeatures                 -> "all"
        Administrative              -> "administrative"
        AdministrativeCountry       -> "administrative.country"
        AdministrativeLandParcel    -> "administrative.land_parcel"
        AdministrativeLocality      -> "administrative.locality"
        AdministrativeNeighborhood  -> "administrative.neighborhood"
        AdministrativeProvince      -> "administrative.province"
        Landscape                   -> "landscape"
        LandscapeManMade            -> "landscape.man_made"
        LandscapeNatural            -> "landscape.natural"
        LandscapeNaturalLandcover   -> "landscape.landcover"
        LandscapeNaturalTerrain     -> "landscape.terrain"
        Poi                         -> "poi"
        PoiAttraction               -> "poi.attraction"
        PoiBusiness                 -> "poi.business"
        PoiGovernment               -> "poi.government"
        PoiMedical                  -> "poi.medical"
        PoiPark                     -> "poi.park"
        PoiPlaceOfWorship           -> "poi.place_of_worship"
        PoiSchool                   -> "poi.school"
        PoiSportsComplex            -> "poi.sports_complex"
        Road                        -> "road"
        RoadArterial                -> "road.arterial"
        RoadHighway                 -> "road.highway"
        RoadHighwayControlledAccess -> "road.controlled_access"
        RoadLocal                   -> "road.local"
        Transit                     -> "transit"
        TransitLine                 -> "transit.line"
        TransitStation              -> "transit.station"
        TransitStationAirport       -> "transit.station.airport"
        TransitStationBus           -> "transit.station.bus"
        TransitStationRail          -> "transit.station.rail"
        Water                       -> "water"

-- | Feature element
data Element
    = AllElements
    | Geometry
    | GeometryFill
    | GeometryStroke
    | Labels
    | LabelsIcon
    | LabelsText
    | LabelsTextFill
    | LabelsTextStroke
    deriving (Eq, Show)

instance ToHttpApiData Element where
    toUrlPiece element = case element of
        AllElements      -> "all"
        Geometry         -> "geometry"
        GeometryFill     -> "geometry.fill"
        GeometryStroke   -> "geometry.stroke"
        Labels           -> "labels"
        LabelsIcon       -> "labels.icon"
        LabelsText       -> "labels.text"
        LabelsTextFill   -> "labels.text.fill"
        LabelsTextStroke -> "labels.text.stroke"

-- | Map style operation
data MapStyleOp
    = StyleHue Word8 Word8 Word8
    | StyleLightness Double
    | StyleSaturation Double
    | StyleGamma Double
    | StyleInvertLightness Bool
    | StyleVisibility Visibility
    | StyleColor Word8 Word8 Word8
    | StyleWeight Int
    deriving (Eq, Show)

instance ToHttpApiData MapStyleOp where
    toUrlPiece mapStyleOp
        | StyleHue r g b <- mapStyleOp
          = T.pack $ "hue:0x" ++ hexString r ++ hexString g ++ hexString b
        | StyleLightness l <- mapStyleOp
          = T.concat ["lightness:", toUrlPiece l]
        | StyleSaturation s <- mapStyleOp
          = T.concat ["saturation:", toUrlPiece s]
        | StyleGamma g <- mapStyleOp
          = T.concat ["gamma:", toUrlPiece g]
        | StyleInvertLightness i <- mapStyleOp
          = T.concat ["invert_lightness:", toUrlPiece i]
        | StyleVisibility e <- mapStyleOp
          = T.concat ["visibility:", toUrlPiece e]
        | StyleColor r g b <- mapStyleOp
          = T.pack $ "color:0x" ++ hexString r ++ hexString g ++ hexString b
        | StyleWeight w <- mapStyleOp
          = T.concat ["weight:", toUrlPiece w]

instance ToHttpApiData [MapStyleOp] where
    toUrlPiece ops = T.concat $ intersperse pipe $ map toUrlPiece ops
      where
        pipe = toUrlPiece ("|" :: Text)

-- | Visibility
data Visibility
    = On
    | Off
    | Simplified  -- ^ Removes some, not all, style features
    deriving (Eq, Show)

instance ToHttpApiData Visibility where
    toUrlPiece visibility = case visibility of
        On         -> "on"
        Off        -> "off"
        Simplified -> "simplified"

-- | Markers
data Markers = Markers (Maybe MarkerStyle) [Location]
    deriving (Eq, Show)

instance ToHttpApiData Markers where
    toUrlPiece (Markers markerStyleOpt ls)
        | Nothing <- markerStyleOpt
          = toUrlPiece ls
        | Just (StdMarkerStyle Nothing Nothing Nothing) <- markerStyleOpt
          = toUrlPiece ls
        | Just markerStyle <- markerStyleOpt
          = case ls of
                [] -> toUrlPiece markerStyle
                _  -> T.concat [toUrlPiece markerStyle, "|", toUrlPiece ls]

-- | Marker style
data MarkerStyle
    = StdMarkerStyle
          { markerSize  :: Maybe MarkerSize
          , markerColor :: Maybe MarkerColor
          , markerLabel :: Maybe MarkerLabel
          }
    | CustomIcon
          { icon   :: URI
          , anchor :: Maybe Anchor
          }
    deriving (Eq, Show)

instance ToHttpApiData MarkerStyle where
    toUrlPiece markerStyle
        | StdMarkerStyle ms mc ml <- markerStyle
          = let size'  = T.append "size:" . toUrlPiece <$> ms
                color' = T.append "color:" . toUrlPiece <$> mc
                label' = T.append "label:" . toUrlPiece <$> ml
                opts     = catMaybes [size', color', label']
            in  T.concat $ intersperse pipe opts
        | CustomIcon url ma <- markerStyle
          = let icon' = T.concat ["icon:", toUrlPiece $ uriToString id url ""]
            in  case ma of
                    Nothing -> icon'
                    Just a -> T.concat [icon', pipe, "anchor:", toUrlPiece a]
      where
        pipe = toUrlPiece ("|" :: Text)

-- | Marker size
data MarkerSize
    = Tiny
    | Mid
    | Small
    deriving (Eq, Show)

instance ToHttpApiData MarkerSize where
    toUrlPiece markerSize' = case markerSize' of
        Tiny  -> "tiny"
        Mid   -> "mid"
        Small -> "small"

-- | Marker colour
data MarkerColor
    = MarkerColor Word8 Word8 Word8
    | StdMarkerColor StdColor
    deriving (Eq, Show)

instance ToHttpApiData MarkerColor where
    toUrlPiece (MarkerColor r g b) = T.pack $ "0x" ++ hexString r ++ hexString g
        ++ hexString b
    toUrlPiece (StdMarkerColor stdColor) = toUrlPiece stdColor

-- | Standard colours
data StdColor
    = Black
    | Brown
    | Green
    | Purple
    | Yellow
    | Blue
    | Gray
    | Orange
    | Red
    | White
    deriving (Eq, Show)

instance ToHttpApiData StdColor where
    toUrlPiece stdColor = case stdColor of
        Black  -> "black"
        Brown  -> "brown"
        Green  -> "green"
        Purple -> "purple"
        Yellow -> "yellow"
        Blue   -> "blue"
        Gray   -> "gray"
        Orange -> "orange"
        Red    -> "red"
        White  -> "white"

-- | Marker label character
newtype MarkerLabel = MarkerLabel Char
    deriving (Eq, Show, ToHttpApiData)

-- | Anchor
data Anchor
    = AnchorPoint Int Int
    | StdAnchor StdAnchor
    deriving (Eq, Show)

instance ToHttpApiData Anchor where
    toUrlPiece anchor
        | AnchorPoint x y <- anchor
          = T.pack (show x ++ "," ++ show y)
        | StdAnchor stdAnchor <- anchor
          = toUrlPiece stdAnchor

-- | Standard anchor points
data StdAnchor
    = AnchorTop
    | AnchorBottom
    | AnchorLeft
    | AnchorRight
    | AnchorCenter
    | AnchorTopLeft
    | AnchorTopRight
    | AnchorBottomLeft
    | AnchorBottomRight
    deriving (Eq, Show)

instance ToHttpApiData StdAnchor where
    toUrlPiece stdAnchor = case stdAnchor of
        AnchorTop         -> "top"
        AnchorBottom      -> "bottom"
        AnchorLeft        -> "left"
        AnchorRight       -> "right"
        AnchorCenter      -> "center"
        AnchorTopLeft     -> "topleft"
        AnchorTopRight    -> "topright"
        AnchorBottomLeft  -> "bottomleft"
        AnchorBottomRight -> "bottomright"

-- | Path
data Path = Path (Maybe PathStyle) [Location]
    deriving (Eq, Show)

instance ToHttpApiData Path where
    toUrlPiece (Path pathStyleOpt ls)
        | Nothing <- pathStyleOpt
          = toUrlPiece ls
        | Just (PathStyle Nothing Nothing Nothing Nothing) <- pathStyleOpt
          = toUrlPiece ls
        | Just pathStyle <- pathStyleOpt
          = case ls of
                [] -> toUrlPiece pathStyle
                _  -> T.concat [toUrlPiece pathStyle, "|", toUrlPiece ls]

-- | Path style: a geodesic path follows the curvature of the Earth.
data PathStyle = PathStyle
    { pathWeight     :: Maybe PathWeight    -- ^ The default value is 5.
    , pathColor      :: Maybe PathColor
    , pathFillColor  :: Maybe PathColor
    , pathGeodesic   :: Maybe PathGeodesic  -- ^ The default value is false.
    } deriving (Eq, Show)

instance ToHttpApiData PathStyle where
    toUrlPiece (PathStyle mw mc mfc mg) =
        T.concat $ intersperse pipe opts
      where
        pipe         = toUrlPiece ("|" :: Text)
        weightUrl    = T.append "weight:" . toUrlPiece <$> mw
        colorUrl     = T.append "color:" . toUrlPiece <$> mc
        fillColorUrl = T.append "fillcolor:" . toUrlPiece <$> mfc
        geodesicUrl  = T.append "geodesic:" . toUrlPiece <$> mg
        opts         = catMaybes [weightUrl, colorUrl, fillColorUrl,
                           geodesicUrl]

-- | Path weight: in pixels.
newtype PathWeight = PathWeight Int
    deriving (Eq, Show, ToHttpApiData)

-- | Path colour
data PathColor
    = PathColor Word8 Word8 Word8
    | PathColorAlpha Word8 Word8 Word8 Word8
    | StdPathColor StdColor
    deriving (Eq, Show)

instance ToHttpApiData PathColor where
    toUrlPiece (PathColor r g b) = T.pack $ "0x" ++ hexString r ++ hexString g
        ++ hexString b
    toUrlPiece (PathColorAlpha r g b a) = T.pack $ "0x" ++ hexString r ++
        hexString g ++ hexString b ++ hexString a
    toUrlPiece (StdPathColor stdColor) = toUrlPiece stdColor

-- | Path is geodesic
newtype PathGeodesic = PathGeodesic Bool
    deriving (Eq, Show, ToHttpApiData)

-- | Visible locations
newtype Visible = Visible [Location]
    deriving (Eq, Show, ToHttpApiData)

-- | Google Static Maps API
type GoogleStaticMapsAPI
    =  "staticmap"
    :> QueryParam "key" Key
    :> QueryParam "signature" Signature
    :> QueryParam "center" Center
    :> QueryParam "zoom" Zoom
    :> QueryParam "size" Size
    :> QueryParam "scale" Scale
    :> QueryParam "format" Format
    :> QueryParams "style" MapStyle
    :> QueryParam "maptype" MapType
    :> QueryParams "markers" Markers
    :> QueryParams "path" Path
    :> QueryParam "visible" Visible
    :> Get '[PNG] StaticmapResponse

-- | StaticmapResponse
type StaticmapResponse = DynamicImage

-- | API type
api :: Proxy GoogleStaticMapsAPI
api = Proxy

staticmap'
    :: Maybe Key
    -> Maybe Signature
    -> Maybe Center
    -> Maybe Zoom
    -> Maybe Size
    -> Maybe Scale
    -> Maybe Format
    -> [MapStyle]
    -> Maybe MapType
    -> [Markers]
    -> [Path]
    -> Maybe Visible
    -> ClientM StaticmapResponse
staticmap' = client api

googleMapsApis :: BaseUrl
googleMapsApis = BaseUrl Https "maps.googleapis.com" 443 "/maps/api"

-- | Retrieve a static map. NB: The use of the Google Static Maps API services
-- is subject to the <https://developers.google.com/maps/terms Google Maps APIs Terms of Service>.
staticmap
    :: Manager
    -> Key
    -> Maybe Signature
    -> Maybe Center
    -> Maybe Zoom
    -> Size
    -> Maybe Scale
    -> Maybe Format
    -> [MapStyle]
    -> Maybe MapType
    -> [Markers]
    -> [Path]
    -> Maybe Visible
    -> IO (Either ServantError StaticmapResponse)
staticmap
    mgr
    key
    signatureOpt
    centerOpt
    zoomOpt
    size
    scaleOpt
    formatOpt
    mapStyles
    mapTypeOpt
    markerss
    paths
    visibleOpt
    = runClientM (staticmap' (Just key) signatureOpt centerOpt zoomOpt
          (Just size) scaleOpt formatOpt mapStyles mapTypeOpt markerss paths
          visibleOpt)
          (ClientEnv mgr googleMapsApis)
