---
editor_options:
  markdown:
    wrap: 72
---

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
