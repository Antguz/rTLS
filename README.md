# rTLS


This is an R package to that compiles a series of tools to process and calculate metrics on point clouds derived from terrestrial LiDAR (Light Detection and Ranging) data.

Originally, *rTLS* was created by J. Antonio Guzman Q. (<antguz06@gmail.com>) at the University of Alberta under the supervison of Dr. Arturo Sanchez (<arturo.sanchez@ualberta.ca>) and the supreme help of Ronny Hernandez (<ronny.hernandezm@gmail.com>). It compiles several functions that students at the [Centre for Earth Observation Sciences](https://www.ualberta.ca/faculties/centresinstitutes/centre-for-earth-observation-sciences) use to process their point cloud of vegetation.

The current development of *rTLS* is focussed on four major processes aplied on point clouds: i) filtering, ii) voxelization, iii) point neigborhood features, and iv) optimal voxel or sphere size. Specifically, the structure of the functions of *rTLS* can be aggruped as:

![](https://user-images.githubusercontent.com/7254767/51277964-6b730680-1996-11e9-97ea-3360bb47b51e.png)

### Major features include:

* Support for major platforms: Windows, Linux, macOS.

* Fast computation using parallel processing options.

* Different options for filtering (eg. SOR, minimun number of points, mean distance)

* Fast voxelization using grid base methods.

* Estimation of neigboring points using two approach: 
    + sphere neighborhood. 
    + k-nearest neighbors.

* Calculation of basic metrics on neigboring points.

* Calculation of the dispersion, aggregation, and mean distance of the neigboring points.

* Estimation of dimensionality.

* Estimation of the optimal voxel and sphere size using information theory.


### Installation


You can install the rTLS using the CRAN plataform following

```{r}
install.packages("rTLS")
```

or using the development version in github following

```{r}
#install.packages("devtools")
devtools::install_github("Antguz/rTLS")
```

### Usage

Currently rTLS
