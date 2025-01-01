---
editor_options:
  markdown:
    wrap: 72
---

# SunCalcMeeus 0.1.2

- Add function `irrad_extraterrestrial()` implementing estimation of the
(shortwave) energy irradiance at the top of the atmosphere above an specific
geographic location and instant in time. The irradiance is estimate on a plane
tangential to the surface of the "sphere", thus, it is corrected for angle of
incidence and for the current Earth-Sun distance.

# SunCalcMeeus 0.1.1

- Add function `relative_AMt()` implementing Young's (1994) AM approximation based
on the true position of the sun.
- Add `relative_AM_geotime()` and `relative_AMt_geotime()`as convenience 
functions.
- Increase test coverage to nearly 95% with new tests and fix some minor bugs.
- Fix bug in `distance_to_sun()`, value returned was totally off-mark.

# SunCalcMeeus 0.1.0

- Package created as a spin-off from 'photobiology' by moving the astronomical
computations related to the sun position unchanged.
