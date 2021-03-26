## Test environments
* Ubuntu 20.04, R 4.0.4, local machine
* Debian Linux, R-devel, GCC (2)
* Windows Server 2008 R2 SP1, R-release, 32/64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release 0.2.3.1
Minor changes were added to the package to safely retain it on CRAN. A line on 'LazyDataCompression: bzip2' was added to DESCRIPTION as recommended by 
Prof Brian Ripley on his email (2020-03-26). In addition, the potential problems with
testthat("rTLS") were modified removing the 'context()' on each test file. 



Only one note regarding of the package size in my local machine.


