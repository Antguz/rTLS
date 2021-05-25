## Test environments
* Ubuntu 20.04, R 4.1.0, local machine

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release 0.2.3.4
Minor changes were added to the package to safely retain it on CRAN. We modify the C++ to remove the 'gcc-ASAN' issue. We did that modifying the raw pointer to memory of the C++ code. We hope that this works, we were not able to test or reproduce the gcc-ASAN issue.

Only one note regarding of the package size in my local machine.


