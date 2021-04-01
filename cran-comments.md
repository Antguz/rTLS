## Test environments
* Ubuntu 20.04, R 4.0.4, local machine

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release 0.2.3.2
Minor changes were added to the package to safely retain it on CRAN. A line on 'LazyDataCompression: bzip2' was added to DESCRIPTION as recommended by Prof Brian Ripley on his email (2020-03-26). In addition, the potential problems with testthat("rTLS") were modified removing the 'context()' on each test file. We suspect that the issue on 'gcc-ASAN' was due to the version of the 'data.table' package that we were using. We suspect so since the C/C++ code used in the function 'filter' where CRAN reported the error is also used in other functions with no error. We are not 100% sure that the error was correct due to we can not see any error using Rhub. We will appreciate any help on this.

Only one note regarding of the package size in my local machine.


