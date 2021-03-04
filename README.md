
rTLS <img src="https://raw.githubusercontent.com/Antguz/rTLS/master/man/figures/rTLS_logo.png" align="right" alt="" width="120" />
=============================================================================
[![DOI](https://zenodo.org/badge/162520913.svg)](https://zenodo.org/badge/latestdoi/162520913)
[![R build status](https://github.com/Antguz/rTLS/workflows/R-CMD-check/badge.svg)](https://github.com/Antguz/rTLS/actions)
[![Codecov test coverage](https://codecov.io/gh/Antguz/rTLS/branch/master/graph/badge.svg)](https://codecov.io/gh/Antguz/rTLS?branch=master)

This is an R package to that compiles a series of tools to process and
calculate metrics on point clouds derived from terrestrial LiDAR (Light
Detection and Ranging) data.

Originally, ***rTLS*** started as a hobby by [J. Antonio Guzman Q.](https://www.jaguzmanq.com/) (<antguz06@gmail.com>) at the University
of Alberta. It compiles several functions that students at the [Centre
for Earth Observation
Sciences](https://apps.ualberta.ca/directory/person/gasanche)
use to process their point cloud of vegetation. Due to its constant use,
it was decided to formalize its routines as an R package. This was done
under the supervision of [Dr. Arturo  Sanchez](https://www.ualberta.ca/science/about-us/contact-us/faculty-directory/arturo-sanchez-azofeifa)
(<arturo.sanchez@ualberta.ca>) and the supreme help of [Ronny Hernandez](http://ronnyhdez.rbind.io/) (<ronny.hernandezm@gmail.com>).

***rTLS*** goal is to provide a single environment to process point
clouds as fast as it can be done. That it is why the ***rTLS*** coding
has been evolving using different packages. The current code has been
enhanced using C++ language through `Rcpp` and `RcppArmadillo` packages,
and most of the time-consuming processed can be run using for parallel
computing through `OpenMP`. In the future, if there is a faster way to
run our main routines, we will probably change our code!

The current development of ***rTLS*** is focused on five major processes
applied on TLS scans: i) voxelization, ii) point neighborhood features,
iii) filtering, and iv) optimal voxel and entropy, and iv) estimation of
the forest structure. Likewise, the package also contains other
functions that can be used to mange point clouds such as 2D-
3D-rotation, conversion to cardinal-polar coordinates, and functions to
estimate tree metrics such as truck volume or crown area.

-----

### Major features include:

  - Support for major platforms: Windows, Linux, macOS.

  - Fast computation using parallel processing options.

  - Fast voxelization using grid base methods.

  - Applications of voxel-counting methods.

  - Estimation of dimensionality of the neighboring points using two
    approach:
    
      - sphere neighborhood.
      - k-nearest neighbors.

  - Calculation of basic metrics on neighboring points.

  - Estimation of dimensionality.

  - Estimation of the optimal voxel and sphere size using information
    theory (coming soon).

-----

### Installation

You can install the ***rTLS*** using the CRAN platform following
(pending)

```
install.packages("rTLS")
```

or using the development version in github following (recommended)

```
#install.packages("devtools")
devtools::install_github("Antguz/rTLS")
```

if you have problems with `devtools` also try

```
devtools::install_github("Antguz/rTLS", INSTALL_opts= c("--no-multiarch"))
```

-----

### Usage

Take a look to the [vignettes](https://antguz.github.io/rTLS/articles/)
were we provide detaits for the use and potential applications of our
functions.
