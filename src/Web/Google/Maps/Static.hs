{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Web.Google.Maps.Static
-- Description : Bindings to the Google Maps Static API (formerly Static Maps
--               API)
-- Copyright   : (c) Mike Pilgrem 2017, 2018
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
--
-- This module has no connection with Google Inc. or its affiliates.
--
-- The <https://developers.google.com/maps/documentation/maps-static/intro Google Maps Static API>
-- returns a map as an image via an HTTP request. This library provides bindings
-- in Haskell to that API (version 2).
--
-- NB: The use of the Google Maps Static API services is subject to the
-- <https://cloud.google.com/maps-platform/terms/ Google Maps Platform Terms of Service>,
-- which terms restrict the use of content. End Users' use of Google Maps is
-- subject to the then-current Google Maps/Google Earth Additional Terms of
-- Service at <https://maps.google.com/help/terms_maps.html> and Google Privacy
-- Policy at <https://www.google.com/policies/privacy/>.
--
-- The following are not yet implemented: non-PNG image formats; and encoded
-- polyline paths.
--
-- The code below is an example console application to test the use of the
-- library with the Google Maps Static API.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Data.Maybe (fromJust)
-- > import Graphics.Gloss (Display (..), display, white)  -- package gloss
-- > import Graphics.Gloss.Juicy (fromDynamicImage)        -- package gloss-juicy
-- > import Network.HTTP.Client (newManager)
-- > import Network.HTTP.Client.TLS (tlsManagerSettings)
-- > import Web.Google.Maps.Static (Center (..), Key (..), Location (..), Size (..),
-- >     staticmap, StaticmapResponse (..), Zoom (..))
-- >
-- > main :: IO ()
-- > main = do
-- >   putStrLn $ "A test of the Google Maps Static API.\nNB: The use of " ++
-- >     "the API services is subject to the Google Maps Platform Terms of " ++
-- >     "Serivce at https://cloud.google.com/maps-platform/terms/.\n"
-- >   mgr <- newManager tlsManagerSettings
-- >   let apiKey = Key "<REPLACE_THIS_WITH_YOUR_ACTUAL_GOOGLE_API_KEY>"
-- >       -- If using a digital signature ...
-- >       secret = Just $ Secret
-- >         "<REPLACE_THIS_WITH_YOUR_ACTUAL_GOOGLE_URL_SIGNING_SECRET>"
-- >       center = Just $ Center (Location 42.165950 (-71.362015))
-- >       zoom   = Just $ Zoom 17
-- >       w      = 400
-- >       h      = 400
-- >       size   = Size w h
-- >   result <- staticmap mgr apiKey secret center zoom size Nothing Nothing
-- >               [] Nothing Nothing Nothing [] [] Nothing
-- >   case result of
-- >     Right response -> do
-- >       let picture = fromJust $ fromDynamicImage response
-- >           title   = "Test Google Maps Static API"
-- >           window  = InWindow title (w, h) (10, 10)
-- >       display window white picture
-- >     Left err -> putStrLn $ "Error! Result:\n" ++ show err
module Web.Google.Maps.Static
       ( -- * Functions
         staticmap
         -- * API
       , GoogleMapsStaticAPI
       , api
         -- * Types
       , Key               (..)
       , Secret            (..)
       , Signature         (..)
       , Center            (..)
       , Location          (..)
       , LatLng            (..)
       , Address           (..)
       , Zoom              (..)
       , Size              (..)
       , Scale             (..)
       , Format            (..)
       , MapStyle          (..)
       , Feature           (..)
       , Element           (..)
       , MapStyleOp        (..)
       , Visibility        (..)
       , MapType           (..)
       , Language          (..)
       , Region            (..)
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
import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert)
import Data.ByteString.Base64.URL (decode, encode)
import Data.ByteString.UTF8 as UTF8 (fromString)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T (append, concat, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Network.HTTP.Client (Manager)
import Network.URI (URI (..), URIAuth (..), uriToString)
import Servant.API ((:>), Get, QueryParam, QueryParams, safeLink,
    ToHttpApiData (..))
import Servant.Client (BaseUrl (..), client, ClientEnv (ClientEnv), ClientM,
    runClientM, ServantError)
import Servant.JuicyPixels (PNG)
import Servant.Links (LinkArrayElementStyle (..), linkURI')
import Text.Bytedump (hexString)
import Web.Google.Maps.Common (Address (..), googleMapsApis, Key (..),
    Language (..), LatLng (..), Location (..), Region (..))

-- | Secret for digital signature
newtype Secret = Secret Text
    deriving (Eq, Show)

-- | Signature
newtype Signature = Signature Text
    deriving (Eq, Show, ToHttpApiData)

-- | Center of the map: not required if the map includes markers or paths.
newtype Center = Center Location
    deriving (Eq, Show, ToHttpApiData)

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

-- | MapStyle
data MapStyle = MapStyle (Maybe Feature) (Maybe Element) [MapStyleOp]
    deriving (Eq, Show)

instance ToHttpApiData MapStyle where
    toUrlPiece (MapStyle featureOpt elementOpt ops) =
        T.concat $ intersperse "|" $ catMaybes [featureUrl, elementUrl] ++
            [opsUrl]
      where
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
    | AllGeometry
    | GeometryFill
    | GeometryStroke
    | AllLabels
    | LabelsIcon
    | LabelsText
    | LabelsTextFill
    | LabelsTextStroke
    deriving (Eq, Show)

instance ToHttpApiData Element where
    toUrlPiece element = case element of
        AllElements      -> "all"
        AllGeometry      -> "geometry"
        GeometryFill     -> "geometry.fill"
        GeometryStroke   -> "geometry.stroke"
        AllLabels        -> "labels"
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
    toUrlPiece ops = T.concat $ intersperse "|" $ map toUrlPiece ops

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
            in  T.concat $ intersperse "|" opts
        | CustomIcon url ma <- markerStyle
          = let icon' = T.concat ["icon:", toUrlPiece $ uriToString id url ""]
            in  case ma of
                    Nothing -> icon'
                    Just a -> T.concat [icon', "|", "anchor:", toUrlPiece a]

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
    toUrlPiece anchor'
        | AnchorPoint x y <- anchor'
          = T.pack (show x ++ "," ++ show y)
        | StdAnchor stdAnchor <- anchor'
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
        T.concat $ intersperse "|" opts
      where
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

-- | Google Maps Static API
type GoogleMapsStaticAPI
    =  "staticmap"
    :> QueryParam  "key"       Key
    :> QueryParam  "center"    Center
    :> QueryParam  "zoom"      Zoom
    :> QueryParam  "size"      Size
    :> QueryParam  "scale"     Scale
    :> QueryParam  "format"    Format
    :> QueryParams "style"     MapStyle
    :> QueryParam  "maptype"   MapType
    :> QueryParam  "language"  Language
    :> QueryParam  "region"    Region
    :> QueryParams "markers"   Markers
    :> QueryParams "path"      Path
    :> QueryParam  "visible"   Visible
    :> QueryParam  "signature" Signature
    :> Get '[PNG] StaticmapResponse

-- | StaticmapResponse
type StaticmapResponse = DynamicImage

-- | API type
api :: Proxy GoogleMapsStaticAPI
api = Proxy

staticmap'
    :: Maybe Key
    -> Maybe Center
    -> Maybe Zoom
    -> Maybe Size
    -> Maybe Scale
    -> Maybe Format
    -> [MapStyle]
    -> Maybe MapType
    -> Maybe Language
    -> Maybe Region
    -> [Markers]
    -> [Path]
    -> Maybe Visible
    -> Maybe Signature
    -> ClientM StaticmapResponse
staticmap' = client api

-- | Retrieve a static map. NB: The use of the Google Maps Static API services
-- is subject to the
-- <https://cloud.google.com/maps-platform/terms/ Google Maps Platform Terms of Service>.
-- End Users' use of Google Maps is subject to the then-current Google
-- Maps/Google Earth Additional Terms of Service at
-- <https://maps.google.com/help/terms_maps.html> and Google Privacy Policy at
-- <https://www.google.com/policies/privacy/>.
staticmap
    :: Manager
    -> Key
    -> Maybe Secret
    -> Maybe Center
    -> Maybe Zoom
    -> Size
    -> Maybe Scale
    -> Maybe Format
    -> [MapStyle]
    -> Maybe MapType
    -> Maybe Language
    -> Maybe Region
    -> [Markers]
    -> [Path]
    -> Maybe Visible
    -> IO (Either ServantError StaticmapResponse)
staticmap
    mgr
    key
    secretOpt
    centerOpt
    zoomOpt
    size
    scaleOpt
    formatOpt
    mapStyles
    mapTypeOpt
    languageOpt
    regionOpt
    markerss
    paths
    visibleOpt
    = case secretOpt of
          Nothing -> runClientM (eval staticmap' Nothing)
-- CookieJar supported from servant-client-0.13
#if MIN_VERSION_servant_client(0,13,0)
                                (ClientEnv mgr googleMapsApis Nothing)
#else
                                (ClientEnv mgr googleMapsApis)
#endif
          Just secret -> do
              let url = linkURI $ eval (safeLink api api) Nothing
                  signatureOpt = sign secret googleMapsApis url
              runClientM (eval staticmap' signatureOpt)
-- CookieJar supported from servant-client-0.13
#if MIN_VERSION_servant_client(0,13,0)
                         (ClientEnv mgr googleMapsApis Nothing)
#else
                         (ClientEnv mgr googleMapsApis)
#endif
        where
          linkURI = linkURI' LinkArrayElementPlain
          eval f = f (Just key) centerOpt zoomOpt (Just size) scaleOpt formatOpt
                     mapStyles mapTypeOpt languageOpt regionOpt markerss paths
                     visibleOpt

sign :: Secret -> BaseUrl -> URI -> Maybe Signature
sign (Secret secret) baseUrl url = do
    secret' <- either (const Nothing) Just (decode $ encodeUtf8 secret)
    let url'       = UTF8.fromString $ baseUrlPath baseUrl ++ "/" ++ uriToString id url ""
        signature  = hmac secret' url' :: HMAC SHA1
        signature' = decodeUtf8 $ encode $ convert signature
    return $ Signature signature'
