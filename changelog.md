# 0.7.0.0

* Add additional languages
* Update dependencies for GHC 8.10.4 and revisit upper bounds

# 0.6.0.1

* Update dependencies for GHC 8.8.1

# 0.6.0.0

* Update documentation to reflect changes to Google Maps Platform

* Rename module `Web.Google.Static.Maps` as `Web.Google.Maps.Static` and
  `GoogleStaticMapsAPI` as `GoogleMapsStaticAPI`

* Update `Web.Google.Maps.Common.Language` data constructors

* Update dependencies for GHC 8.6.2 and revisit upper bounds

# 0.5.0.3

* Update dependencies for GHC 8.6.1

# 0.5.0.2

* Update dependencies for GHC 8.4.3

# 0.5.0.1

* Update dependencies for GHC 8.2.1 and relax lower bounds

# 0.5.0.0

* Depend on more recent version of `servant` package and, consequently,
  `servant-client`, `aeson` and `cryptonite` packages

# 0.4.0.0

* Implement `language` and `region` parameters

* Implement digital signatures

# 0.3.0.0

* Fix bug in instance of `ToHttpApiData` for `Location`

* Rename certain constructors of `Element` type to avoid name clashes
  (`Geometry` and `Labels`)

* Move functions and types of anticipated common use to module
  `Web.Google.Maps.Common`

# 0.2.0.0

* Implement `signature` and custom marker icons

* Move `staticmap` from the end of the base URL to the start of the API type

* Modify `Show` instances for certain types (`Element`, `Feature`,
  `MarkerColor`, `PathColor`, `StdColor`, `Visibility`)

# 0.1.0.0

* Launch implementation. Not yet implemented: certain optional parameters
  (`language`, `region` and `signature`); address locations; non-PNG image
  formats; custom marker icons; and encoded polyline paths
