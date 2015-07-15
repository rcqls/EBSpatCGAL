# EBSpatCGAL

Development tools around nearest-neigbours type Gibbs point processes based on [CGAL](https://www.cgal.org).

## Requirement

EBSpatCGAL is an adaptation of [EBSpat](https://github.com/rcqls/EBSpat) (no more developped) using [CGAL](https://www.cgal.org). 

For MacOSX user: install [brew](http://brew.sh) and then 

	brew install cgal

For Windows user: see install steps described in the last section.

## Install

Since this package is in development, it is not yet on CRAN but thanks to devtools R package (to install first if necessary), you can install it in a R session:

```{.R execute="false"}
devtools::install_github("rcqls/EBSpatCGAL")
```

## examples

```{.R execute="false"}
require(EBSpatCGAL)
del2 <- Delaunay()
insert(del2,x=runif(n<-100),y=runif(n))
plot(del2)
area(del2)		#or volume(del2)
area(del2,3) 	#area of the third point
```

```{.R execute="false"}
require(EBSpatCGAL)
del3 <- Delaunay(3)
insert(del3,x=runif(n<-100),y=runif(n),z=runif(n))
plot(del3)
volume(del3)
volume(del3,3) #area of the third point
```

## Windows install

1) Install [Msys2](https://msys2.github.io) 32bits system is preferred since it is compatible with 64bits system.

2) Then install gcc for MinGW64 and cgal after opening the Mingw32 console (not the msys2 one):

```{bash}
pacman -S mingw-w64-i686-gcc mingw-w64-i686-cgal
```
Also, add the following code in your .bash_profile
```{bash}
export PATH=/mingw32/bin:$PATH
```

3) You also need to recompile Rcpp from source, install devtools and then install EBSpat after opening R (added to PATH) in the console:

```{.R execute="false"}
install.packages("Rcpp",type="source")
install.packages(‘devtools’)
devtools::install_github('rcqls/EBSpatCGAL’)
```
4) If you want to use the package inside the Rgui.exe or Rstudio, add C:\msys2\mingw32\bin in the Windows PATH (where C:\msys2 is supposed here to be where Msys2 is installed).
