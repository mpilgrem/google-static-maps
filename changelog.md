# 0.3.0.0

* Fix bug in instance of `ToHttpApiData` for `Location`

* Move functions and types of anticipated common use to module
  `Web.Google.Maps.Common`

# 0.2.0.0

* Implementation of `signature` and custom marker icons

* Move `staticmap` from the end of the base URL to the start of the API type

* Modify `Show` instances for certain types (`Element`, `Feature`,
  `MarkerColor`, `PathColor`, `StdColor`, `Visibility`)

# 0.1.0.0

* Launch implementation. Not yet implemented: certain optional parameters
  (`language`, `region` and `signature`); address locations; non-PNG image
  formats; custom marker icons; and encoded polyline paths
