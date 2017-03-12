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
