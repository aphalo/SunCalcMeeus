---
editor_options:
  markdown:
    wrap: 72
---

# SunCalcMeeus 0.1.2

- Add function `irrad_extraterrestrial()` implementing estimation of the
(shortwave) energy irradiance at the top of the atmosphere above a specific
geographic location and at a given instant in time. The irradiance is estimated
on a plane tangential to the surface of the "sphere" or Earth globe. Thus, it
returns the solar constant corrected for the angle of incidence and for the
current Earth-Sun distance.
- Add on-line only article with accuracy and performance tests based on
comparisons to alternatives.

# SunCalcMeeus 0.1.1

- Add function `relative_AMt()` implementing Young's (1994) AM approximation 
based on the true position of the sun.
- Add `relative_AM_geotime()` and `relative_AMt_geotime()`as convenience 
wrapper functions.
- Increase test coverage to nearly 95% with new tests and fix some minor bugs.
- Fix bug in `distance_to_sun()`, the value returned was totally off-mark.

# SunCalcMeeus 0.1.0

- Package created as a spin-off from 'photobiology' by moving the astronomical
computations related to the sun position, unchanged.
