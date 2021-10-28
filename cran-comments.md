## R CMD check results

0 errors | 0 warnings | 1 note

Just a note about the file size like previous version. In this new version we modified the /Makewars as a potential source of error that triggers the gcc-ASAN. If this does not work, it is likely that this is a false positive by triggered using '#pragma omp parallel' in RcppArmadillo as described in 'https://github.com/RcppCore/RcppArmadillo/issues/189'. The package has been out of cran for months, we don't know what else we can do. Thanks.
