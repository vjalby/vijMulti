# vijMulti 1.1.2 (2026-11-08)
* CA: missing, negative or infinite counts are now always rejected; zero counts are treated as no observation
* PCA: a missing group is shown as "(Missing)" instead of "NA" (legend) or an empty cell (Observations table)
* CA/MCA: dimensions with null inertia (e.g. proportional rows, redundant variables) are still listed in the Summary table but can no longer be used as dimensions
* MCA: fixed an error with variable names containing spaces or special characters
* CA: fixed a column missing from the tables when a supplementary column, once marked with "*", had the same name as an active one
* CA: the χ² Test table now shows a note when some expected counts are below 5
* CA: decimal supplementary row/column numbers (e.g. 2.9) are now rejected instead of being truncated
* PCA: a variable measured in very small units is no longer wrongly rejected as constant


# vijMulti 1.1.1 (2026-10-16)
* MCA: Fixed total inertia for Greenacre correction
* small fixes and optimizations

# vijMulti 1.1.0 (2026-10-09)
* CA/MCA computations are now based on Greenacre's ca package
* CA: Improved handling of supplementary points in profile tables (SAS-like)
* MCA: Analysis now possible with only two variables
* Some ggsci color palettes added

# vijMulti 1.0.2 (2026-09-29)
* Code optimization and fixes

# vijMulti 1.0.1 (2026-09-27)
* Added χ² Test table to CA output
* Reordered/renamed CA summary columns
* Improved support for decimal weights (CA)
* Code optimization and fixes

# vijMulti 1.0.0 (2026-09-17)
* The three analyses PCA, CA and MCA have been moved from vijPlots to this new module
* MCA (Burt method): Fixed variable discrimination computation
* Two new color palettes based on the Carbon Design System (Carbon:Dark, Carbon:Light)
* Improved ggplot2 4.0 support
* Improved error handling
* Code optimization and fixes

