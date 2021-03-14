## Test environments
* Ubuntu 20.04, R 4.0.4, local machine (1)
* Debian Linux, R-devel, GCC (2)
* Windows Server 2008 R2 SP1, R-release, 32/64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release 0.2.2.
Only one note related to the installed size (data and libs). We reduce the 
size the examples considerably in compassion with 0.2.1. However, the size still 
greater than 5Mb; currently, it is 23.4Mb. The C++ code complied also contributes 
to the package size. We consider that we need an exception of the package size. 

We added the \value to .Rd files regarding exported methods. We also corrected 
the \dontrun{} with \donttest{} in those examples that take more than > 5s. In 
this new version, we added and corrected all the authors, contributions, and 
copyright holders in the DESCRIPTION file. References in some functions for 
some methods are properly described in each function. These references do not 
need to be included in the DESCRIPTION file given the amount of them and the 
specificity of the method in question that does not correspond with our 
package's core goal. Thanks.



