# rTLS 0.2.6.1

We move from sp to sf package.

# rTLS 0.2.5.2

Bug on package test

# rTLS 0.2.5

Having FLANN all this time has causing gcc-ASAN problems, a headache for us and 
more than four months outside of CRAN. This new version, 'knn' and 'radius_search' 
were modified to work with 'RcppHNSW' as a base. Problem solved!

# rTLS 0.2.4

Corrections to the compiler

# rTLS 0.2.3.2

Minor corrections on the DESCRIPTION file by adding 'LazyDataCompression: bzip2'
The 'context()' of testthat files was removed.
The 'int' of 'npoints' of circleRANSAC_rcpp was corrected to 'double npoints_double'.
Modifications to the Makevars were done.


# rTLS 0.2.3

rTLS is released in CRAN (2021-03-13)


