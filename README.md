
rTLS <img src="https://raw.githubusercontent.com/Antguz/rTLS/master/man/figures/rTLS_logo.png" align="right" alt="" width="120" />
=============================================================================
[![DOI](https://zenodo.org/badge/162520913.svg)](https://zenodo.org/badge/latestdoi/162520913)
[![R build status](https://github.com/Antguz/rTLS/workflows/R-CMD-check/badge.svg)](https://github.com/Antguz/rTLS/actions)
[![GPLv3 License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)
![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/Antguz/rTLS)
[![Codecov test coverage](https://codecov.io/gh/Antguz/rTLS/branch/master/graph/badge.svg)](https://codecov.io/gh/Antguz/rTLS?branch=master)

This is an R package to that compiles a series of tools to process and calculate metrics on point clouds derived from Terrestrial Laser Scanners (TLS), also know as Terrestrial-Light Detection and Ranging (T-LiDAR).

Originally, ___rTLS___ started as a hobby by [J. Antonio Guzman Q.](https://www.jaguzmanq.com/) (<antguz06@gmail.com>) at the University of Alberta. It compiles several functions that students at the [Centre for Earth Observation Sciences](https://www.ualberta.ca/centre-for-earth-observation-sciences/index.html) use to process their point cloud of vegetation. Due to its constant use, it was decided to formalize its routines as an R package. This was done under the supervision of [Dr. Arturo Sanchez](https://apps.ualberta.ca/directory/person/gasanche) (<arturo.sanchez@ualberta.ca>) and the supreme help of [Ronny Hernandez](http://ronnyhdez.rbind.io/) (<ronny.hernandezm@gmail.com>). 

___rTLS___ goal is to provide a single environment to process point clouds as fast as it can be done. That it is why the ___rTLS___ coding has been evolving using different packages. The current code has been enhanced using C++ language through `Rcpp` and `RcppArmadillo` packages, and most of the time-consuming processed can be run using for parallel computing through `OpenMP`. In the future, if there is a faster way to run our main routines, we will probably change our code!

The current development of ___rTLS___ is focused on five major processes applied on TLS scans: i) voxelization, ii) point neighborhood features, iii) filtering, and iv) optimal voxel and entropy, and iv) estimation of the forest structure. Likewise, the package also contains other functions that can be used to mange point clouds such as 2D- 3D-rotation, conversion to cardinal-polar coordinates, and functions to estimate tree metrics such as truck volume or crown area.

***

### Major features include:

* Support for major platforms: Windows, Linux, macOS.

* Fast computation using parallel processing options.

* Fast voxelization using grid base methods.

* Applications of voxel-counting methods.

* Fast estimation of geometry features of the neighboring points using two approach: 
    + radius search. 
    + k-nearest neighbors.

* Estimation of canopy structure using single point hemispherical scanning

* Filtering of point clouds using:
    + Statistical Outlier Removal (SOR)
    + Minimum of neighbors
    + Nearest point to voxel center

* Line AABB tracing path (ray-tracing)

* Estimation of tree metrics:
    + DBH using RANSAC fitting
    + Crown area using convex hull

* And many more!  

***

### Installation

You can install the ___rTLS___ using the CRAN platform following (pending)

```
install.packages("rTLS")
```

or using the development version in github following (recommended)

```
remotes::install_github("Antguz/rTLS")
```
if you have problems also try 

```
devtools::install_github("Antguz/rTLS", INSTALL_opts= c("--no-multiarch"))
```

Most common problems for installation comes from rgl package. If required, 
try to install rgl first and then ___rTLS___.

***

### Usage

Take a look to the [vignettes](https://antguz.github.io/rTLS/articles/) were we 
provide details for the use and potential applications of our functions.
