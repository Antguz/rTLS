[![Travis build status](https://travis-ci.com/Antguz/rTLS.svg?branch=master)](https://travis-ci.com/Antguz/rTLS)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Antguz/rTLS?branch=master&svg=true)](https://ci.appveyor.com/project/Antguz/rTLS)
[![Codecov test coverage](https://codecov.io/gh/Antguz/rTLS/branch/master/graph/badge.svg)](https://codecov.io/gh/Antguz/rTLS?branch=master)

<p align="right">
  <img width="139" height="150" src="https://user-images.githubusercontent.com/7254767/63565288-a0939880-c525-11e9-9b51-eff30398dba0.png">
</p>

# rTLS


This is an R package to that compiles a series of tools to process and calculate metrics on point clouds derived from terrestrial LiDAR (Light Detection and Ranging) data.

Originally, ___rTLS___ started as a hobby by J. Antonio Guzman Q.  (<antguz06@gmail.com>) at the University of Alberta. However, due to its constant use, it was decided to formalize its routines. This was also done with Dr. Arturo Sanchez (<arturo.sanchez@ualberta.ca>) and the supreme help of Ronny Hernandez (<ronny.hernandezm@gmail.com>). It compiles several functions that students at the [Centre for Earth Observation Sciences](https://www.ualberta.ca/faculties/centresinstitutes/centre-for-earth-observatiTheon-sciences) use to process their point cloud of vegetation.

___rTLS___ goal is to provide a single environment to process point clouds as fast as it can be done. That it is why the ___rTLS___ coding has been evolving using different packages. The current code has been enhanced using `foreach` for parallel procesing with the help of `doSNOW` and `Rcpp` for key functions using C++. In the future, if there is a faster way to run our main routines, we will probably change our code!

The current development of ___rTLS___ is focussed on four major processes applied on TLS scans: i) voxelization, ii) point neighborhood features, and iii) optimal voxel and entropy, iv) estimation of the forest structure. Likewise, the package also containg other functions that can be used to mange point clouds such as 3D-rotation or convertion to cartinal-polar coordinates, and functions to estimate tree metrics such as truck volume or crown area.

### Major features include:

* Support for major platforms: Windows, Linux, macOS.

* Fast computation using parallel processing options.

* Fast voxelization using grid base methods.

* Aplications of voxel-counting methods.

* Estimation of neigboring points using two approach: 
    + sphere neighborhood. 
    + k-nearest neighbors.

* Calculation of basic metrics on neigboring points.

* Calculation of the dispersion, aggregation, and mean distance of the neigboring points.

* Estimation of dimensionality.

* Estimation of the optimal voxel and sphere size using information theory (comming soon).

### Installation

You can install the ___rTLS___ using the CRAN plataform following (pending)

```{r}
install.packages("rTLS")
```

or using the development version in github following (recommended)

```{r}
#install.packages("devtools")
devtools::install_github("Antguz/rTLS")
```

