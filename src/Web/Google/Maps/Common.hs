{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Web.Google.Maps.Common
-- Description : Common to the Google Maps Platform
-- Copyright   : (c) Mike Pilgrem 2017, 2018, 2021
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
--
-- This module has no connection with Google Inc. or its affiliates.
module Web.Google.Maps.Common
    ( -- * Functions
      googleMapsApis
        -- * Types
    , Address  (..)
    , Key      (..)
    , Language (..)
    , LatLng   (..)
    , Location (..)
    , Region   (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Double.Conversion.Text (toFixed)
import Data.Eq (Eq)
import Data.Function (($))
import Data.List (intersperse, map)
import Data.Text (Text)
import qualified Data.Text as T (concat)
import GHC.Exts (Double)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (..))
import Servant.Client (BaseUrl (..), Scheme (..))
import Text.Show (Show)

-- | API key
newtype Key = Key Text
    deriving (Eq, Show, ToHttpApiData)

-- | Location
data Location
    = Coords LatLng
    | Locale Address
    deriving (Eq, Show)

instance ToHttpApiData Location where
    toUrlPiece location
        | Coords latlng <- location
          = toUrlPiece latlng
        | Locale address <- location
          = toUrlPiece address

instance ToHttpApiData [Location] where
    toUrlPiece [] = ""
    toUrlPiece ls = T.concat $ intersperse "|" $ map toUrlPiece ls

-- | Latitude and longitude: precision beyond 6 decimal places is ignored.
data LatLng = LatLng
    { lat :: Double  -- ^ Takes any value between -90 and 90.
    , lng :: Double  -- ^ Takes any value between -180 and 180.
    } deriving (Eq, Show, Generic)

instance ToHttpApiData LatLng where
    toUrlPiece (LatLng lat' lng')
        = T.concat [toFixed precision lat', ",", toFixed precision lng']
      where
        precision = 6  -- Precision beyond 6 decimal places is ignored.

instance FromJSON LatLng

-- | Address
newtype Address = Address Text
    deriving (Eq, Show, ToHttpApiData)

-- | Language: supported languages based on the list at
-- <https://developers.google.com/maps/faq#languagesupport> (as at 12 June
-- 2021).
data Language
    = Afrikaans -- ^ @since 0.7.0.0
    | Albanian
    | Amharic -- ^ @since 0.7.0.0
    | Arabic
    | Armenian -- ^ @since 0.7.0.0
    | Azerbaijani -- ^ @since 0.7.0.0
    | Basque
    | Belarusian
    | Bengali
    | Bosnian -- ^ @since 0.7.0.0
    | Bulgarian
    | Burmese
    | Catalan
    | Chinese -- ^ @since 0.7.0.0
    | ChineseSimplified
    | ChineseHongKong -- ^ @since 0.7.0.0
    | ChineseTraditional
    | Croatian
    | Czech
    | Danish
    | Dutch
    | English
    | EnglishAustralian
    | EnglishBritish
    | Estonian -- ^ @since 0.7.0.0
    | Farsi
    | Filipino
    | Finnish
    | French
    | FrenchCanadian -- ^ @since 0.7.0.0
    | Galician
    | Georgian -- ^ @since 0.7.0.0
    | German
    | Greek
    | Gujarati
    | Hebrew
    | Hindi
    | Icelandic -- ^ @since 0.7.0.0
    | Hungarian
    | Indonesian
    | Italian
    | Japanese
    | Kannada
    | Kazakh
    | Khmer -- ^ @since 0.7.0.0
    | Korean
    | Kyrgyz
    | Lao -- ^ @since 0.7.0.0
    | Latvian
    | Lithuanian
    | Macedonian
    | Malay -- ^ @since 0.7.0.0
    | Malayalam
    | Marathi
    | Mongolian -- ^ @since 0.7.0.0
    | Nepali -- ^ @since 0.7.0.0
    | Norwegian
    | Polish
    | Portuguese
    | PortugueseBrazil
    | PortuguesePortugal
    | Punjabi
    | Romanian
    | Russian
    | Serbian
    | Sinhalese -- ^ @since 0.7.0.0
    | Slovak
    | Slovenian
    | Spanish
    | SpanishLatinAmerican -- ^ @since 0.7.0.0
    | Swahili -- ^ @since 0.7.0.0
    | Swedish
    | Tagalog -- ^ No longer listed by Google at 12 June 2021. See 'Filipino'.
    | Tamil
    | Telugu
    | Thai
    | Turkish
    | Ukrainian
    | Urdu -- ^ @since 0.7.0.0
    | Uzbek
    | Vietnamese
    | Zulu -- ^ @since 0.7.0.0
    deriving (Eq, Show)

instance ToHttpApiData Language where
    toUrlPiece language = case language of
        Afrikaans            -> "af"
        Albanian             -> "sq"
        Amharic              -> "am"
        Arabic               -> "ar"
        Armenian             -> "hy"
        Azerbaijani          -> "az"
        Basque               -> "eu"
        Belarusian           -> "be"
        Bengali              -> "bn"
        Bosnian              -> "bs"
        Bulgarian            -> "bg"
        Burmese              -> "my"
        Catalan              -> "ca"
        Chinese              -> "zh"
        ChineseSimplified    -> "zh-CN"
        ChineseHongKong      -> "zh-HK"
        ChineseTraditional   -> "zh-TW"
        Croatian             -> "hr"
        Czech                -> "cs"
        Danish               -> "da"
        Dutch                -> "nl"
        English              -> "en"
        EnglishAustralian    -> "en-AU"
        EnglishBritish       -> "en-GB"
        Estonian             -> "et"
        Farsi                -> "fa"
        Filipino             -> "fil"
        Finnish              -> "fi"
        French               -> "fr"
        FrenchCanadian       -> "fr-CA"
        Galician             -> "gl"
        Georgian             -> "ka"
        German               -> "de"
        Greek                -> "el"
        Gujarati             -> "gu"
        Hebrew               -> "iw"
        Hindi                -> "hi"
        Hungarian            -> "hu"
        Icelandic            -> "is"
        Indonesian           -> "id"
        Italian              -> "it"
        Japanese             -> "ja"
        Kannada              -> "kn"
        Kazakh               -> "kk"
        Khmer                -> "km"
        Korean               -> "ko"
        Kyrgyz               -> "ky"
        Lao                  -> "lo"
        Latvian              -> "lv"
        Lithuanian           -> "lt"
        Macedonian           -> "mk"
        Malay                -> "ms"
        Malayalam            -> "ml"
        Marathi              -> "mr"
        Mongolian            -> "mn"
        Nepali               -> "ne"
        Norwegian            -> "no"
        Polish               -> "pl"
        Portuguese           -> "pt"
        PortugueseBrazil     -> "pt-BR"
        PortuguesePortugal   -> "pt-PT"
        Punjabi              -> "pa"
        Romanian             -> "ro"
        Russian              -> "ru"
        Serbian              -> "sr"
        Sinhalese            -> "si"
        Slovak               -> "sk"
        Slovenian            -> "sl"
        Spanish              -> "es"
        SpanishLatinAmerican -> "es-419"
        Swahili              -> "sw"
        Swedish              -> "sv"
        Tagalog              -> "tl"
        Tamil                -> "ta"
        Telugu               -> "te"
        Thai                 -> "th"
        Turkish              -> "tr"
        Ukrainian            -> "uk"
        Urdu                 -> "ur"
        Uzbek                -> "uz"
        Vietnamese           -> "vi"
        Zulu                 -> "zu"

-- | Region: a ccTLD (country code top level domain).
data Region
    = AD
    | AE
    | AF
    | AG
    | AI
    | AL
    | AM
    | AO
    | AQ
    | AR
    | AS
    | AT
    | AU
    | AW
    | AX
    | AZ
    | BA
    | BB
    | BD
    | BE
    | BF
    | BG
    | BH
    | BI
    | BJ
    | BL
    | BM
    | BN
    | BO
    | BQ
    | BR
    | BS
    | BT
    | BV
    | BW
    | BY
    | BZ
    | CA
    | CC
    | CD
    | CF
    | CG
    | CH
    | CI
    | CK
    | CL
    | CM
    | CN
    | CO
    | CR
    | CU
    | CV
    | CW
    | CX
    | CY
    | CZ
    | DE
    | DJ
    | DK
    | DM
    | DO
    | DZ
    | EC
    | EE
    | EG
    | EH
    | ER
    | ES
    | ET
    | FI
    | FJ
    | FK
    | FM
    | FO
    | FR
    | GA
    | GB
    | GD
    | GE
    | GF
    | GG
    | GH
    | GI
    | GL
    | GM
    | GN
    | GP
    | GQ
    | GR
    | GS
    | GT
    | GU
    | GW
    | GY
    | HK
    | HM
    | HN
    | HR
    | HT
    | HU
    | ID
    | IE
    | IL
    | IM
    | IN
    | IO
    | IQ
    | IR
    | IS
    | IT
    | JE
    | JM
    | JO
    | JP
    | KE
    | KG
    | KH
    | KI
    | KM
    | KN
    | KP
    | KR
    | KW
    | KY
    | KZ
    | LA
    | LB
    | LC
    | LI
    | LK
    | LR
    | LS
    | LT
    | LU
    | LV
    | LY
    | MA
    | MC
    | MD
    | ME
    | MF
    | MG
    | MH
    | MK
    | ML
    | MM
    | MN
    | MO
    | MP
    | MQ
    | MR
    | MS
    | MT
    | MU
    | MV
    | MW
    | MX
    | MY
    | MZ
    | NA
    | NC
    | NE
    | NF
    | NG
    | NI
    | NL
    | NO
    | NP
    | NR
    | NU
    | NZ
    | OM
    | PA
    | PE
    | PF
    | PG
    | PH
    | PK
    | PL
    | PM
    | PN
    | PR
    | PS
    | PT
    | PW
    | PY
    | QA
    | RE
    | RO
    | RS
    | RU
    | RW
    | SA
    | SB
    | SC
    | SD
    | SE
    | SG
    | SH
    | SI
    | SJ
    | SK
    | SL
    | SM
    | SN
    | SO
    | SR
    | SS
    | ST
    | SV
    | SX
    | SY
    | SZ
    | TC
    | TD
    | TF
    | TG
    | TH
    | TJ
    | TK
    | TL
    | TM
    | TN
    | TO
    | TR
    | TT
    | TV
    | TW
    | TZ
    | UA
    | UG
    | UM
    | US
    | UY
    | UZ
    | VA
    | VC
    | VE
    | VG
    | VI
    | VN
    | VU
    | WF
    | WS
    | YE
    | YT
    | ZA
    | ZM
    | ZW
    | AC -- Saint Helena, Ascension and Tristan da Cunha
    | UK -- United Kingdom of Great Britain and Northern Ireland
    | EU -- European Union
    deriving (Eq, Show)

instance ToHttpApiData Region where
    toUrlPiece region = case region of
        AD -> "ad"
        AE -> "ae"
        AF -> "af"
        AG -> "ag"
        AI -> "ai"
        AL -> "al"
        AM -> "am"
        AO -> "ao"
        AQ -> "aq"
        AR -> "ar"
        AS -> "as"
        AT -> "at"
        AU -> "au"
        AW -> "aw"
        AX -> "ax"
        AZ -> "az"
        BA -> "ba"
        BB -> "bb"
        BD -> "bd"
        BE -> "be"
        BF -> "bf"
        BG -> "bg"
        BH -> "bh"
        BI -> "bi"
        BJ -> "bj"
        BL -> "bl"
        BM -> "bm"
        BN -> "bn"
        BO -> "bo"
        BQ -> "bq"
        BR -> "br"
        BS -> "bs"
        BT -> "bt"
        BV -> "bv"
        BW -> "bw"
        BY -> "by"
        BZ -> "bz"
        CA -> "ca"
        CC -> "cc"
        CD -> "cd"
        CF -> "cf"
        CG -> "cg"
        CH -> "ch"
        CI -> "ci"
        CK -> "ck"
        CL -> "cl"
        CM -> "cm"
        CN -> "cn"
        CO -> "co"
        CR -> "cr"
        CU -> "cu"
        CV -> "cv"
        CW -> "cw"
        CX -> "cx"
        CY -> "cy"
        CZ -> "cz"
        DE -> "de"
        DJ -> "dj"
        DK -> "dk"
        DM -> "dm"
        DO -> "do"
        DZ -> "dz"
        EC -> "ec"
        EE -> "ee"
        EG -> "eg"
        EH -> "eh"
        ER -> "er"
        ES -> "es"
        ET -> "et"
        FI -> "fi"
        FJ -> "fj"
        FK -> "fk"
        FM -> "fm"
        FO -> "fo"
        FR -> "fr"
        GA -> "ga"
        GB -> "gb"
        GD -> "gd"
        GE -> "ge"
        GF -> "gf"
        GG -> "gg"
        GH -> "gh"
        GI -> "gi"
        GL -> "gl"
        GM -> "gm"
        GN -> "gn"
        GP -> "gp"
        GQ -> "gq"
        GR -> "gr"
        GS -> "gs"
        GT -> "gt"
        GU -> "gu"
        GW -> "gw"
        GY -> "gy"
        HK -> "hk"
        HM -> "hm"
        HN -> "hn"
        HR -> "hr"
        HT -> "ht"
        HU -> "hu"
        ID -> "id"
        IE -> "ie"
        IL -> "il"
        IM -> "im"
        IN -> "in"
        IO -> "io"
        IQ -> "iq"
        IR -> "ir"
        IS -> "is"
        IT -> "it"
        JE -> "je"
        JM -> "jm"
        JO -> "jo"
        JP -> "jp"
        KE -> "ke"
        KG -> "kg"
        KH -> "kh"
        KI -> "ki"
        KM -> "km"
        KN -> "kn"
        KP -> "kp"
        KR -> "kr"
        KW -> "kw"
        KY -> "ky"
        KZ -> "kz"
        LA -> "la"
        LB -> "lb"
        LC -> "lc"
        LI -> "li"
        LK -> "lk"
        LR -> "lr"
        LS -> "ls"
        LT -> "lt"
        LU -> "lu"
        LV -> "lv"
        LY -> "ly"
        MA -> "ma"
        MC -> "mc"
        MD -> "md"
        ME -> "me"
        MF -> "mf"
        MG -> "mg"
        MH -> "mh"
        MK -> "mk"
        ML -> "ml"
        MM -> "mm"
        MN -> "mn"
        MO -> "mo"
        MP -> "mp"
        MQ -> "mq"
        MR -> "mr"
        MS -> "ms"
        MT -> "mt"
        MU -> "mu"
        MV -> "mv"
        MW -> "mw"
        MX -> "mx"
        MY -> "my"
        MZ -> "mz"
        NA -> "na"
        NC -> "nc"
        NE -> "ne"
        NF -> "nf"
        NG -> "ng"
        NI -> "ni"
        NL -> "nl"
        NO -> "no"
        NP -> "np"
        NR -> "nr"
        NU -> "nu"
        NZ -> "nz"
        OM -> "om"
        PA -> "pa"
        PE -> "pe"
        PF -> "pf"
        PG -> "pg"
        PH -> "ph"
        PK -> "pk"
        PL -> "pl"
        PM -> "pm"
        PN -> "pn"
        PR -> "pr"
        PS -> "ps"
        PT -> "pt"
        PW -> "pw"
        PY -> "py"
        QA -> "qa"
        RE -> "re"
        RO -> "ro"
        RS -> "rs"
        RU -> "ru"
        RW -> "rw"
        SA -> "sa"
        SB -> "sb"
        SC -> "sc"
        SD -> "sd"
        SE -> "se"
        SG -> "sg"
        SH -> "sh"
        SI -> "si"
        SJ -> "sj"
        SK -> "sk"
        SL -> "sl"
        SM -> "sm"
        SN -> "sn"
        SO -> "so"
        SR -> "sr"
        SS -> "ss"
        ST -> "st"
        SV -> "sv"
        SX -> "sx"
        SY -> "sy"
        SZ -> "sz"
        TC -> "tc"
        TD -> "td"
        TF -> "tf"
        TG -> "tg"
        TH -> "th"
        TJ -> "tj"
        TK -> "tk"
        TL -> "tl"
        TM -> "tm"
        TN -> "tn"
        TO -> "to"
        TR -> "tr"
        TT -> "tt"
        TV -> "tv"
        TW -> "tw"
        TZ -> "tz"
        UA -> "ua"
        UG -> "ug"
        UM -> "um"
        US -> "us"
        UY -> "uy"
        UZ -> "uz"
        VA -> "va"
        VC -> "vc"
        VE -> "ve"
        VG -> "vg"
        VI -> "vi"
        VN -> "vn"
        VU -> "vu"
        WF -> "wf"
        WS -> "ws"
        YE -> "ye"
        YT -> "yt"
        ZA -> "za"
        ZM -> "zm"
        ZW -> "zw"
        AC -> "ac" -- Saint Helena, Ascension and Tristan da Cunha
        UK -> "uk" -- United Kingdom of Great Britain and Northern Ireland
        EU -> "eu" -- European Union

-- | The base URL for the Google Maps Platform APIs.
googleMapsApis :: BaseUrl
googleMapsApis = BaseUrl Https "maps.googleapis.com" 443 "/maps/api"
