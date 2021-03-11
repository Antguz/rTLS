## Test environments
* Ubuntu 20.04, R 4.0.4, local machine (1)
* Debian Linux, R-devel, GCC (2)
* Fedora Linux, R-devel, GCC (3)
* Windows Server 2008 R2 SP1, R-release, 32/64 bit (5)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release 0.2.2.
Only one note related with the installed size (data and libs). We reduce the size 
the examples considerably in compassion with 0.2.1. However the size still greater 
than 5Mb; currently, it is 23.4Mb. The C++ code complied also contributes to the 
package size. We consider that we need an exception of the package size. 

