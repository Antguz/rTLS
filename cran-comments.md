## Test environments
* Ubuntu 20.04, R 4.0.4, local machine (1)
* Debian Linux, R-devel, GCC (2)
* Fedora Linux, R-devel, GCC (3)
* Windows Server 2008 R2 SP1, R-release, 32/64 bit (5)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release 0.2.1.
Only one note related with the installed size (data and libs). The data example 
can not be reduced since the nature of data that cannot be created manually 
for test. Likewise, the C++ code complied also contributes to the package size. 

