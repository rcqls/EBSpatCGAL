#ifndef RCPP_SPATSTAT_TRIANGULATION_H
#define RCPP_SPATSTAT_TRIANGULATION_H
#include "cgal_spatstat_triangulation.h"

#include <Rcpp.h>

//Problem: (HERE, just to remember what happenned!)
//This is simply because typedef is not allowed after class instance (used inside RCPP_EXPOSED_CLASS) 
//class Delaunay2D: public Delaunay2 {};
//RCPP_EXPOSED_CLASS(Delaunay2D);
//class Delaunay3D: public Delaunay3 {};
//RCPP_EXPOSED_CLASS(Delaunay3D)

//Solution: No more use of RCPP_EXPOSED_CLASS but RCPP_EXPOSED_AS and RCPP_EXPOSED_WRAP 

RCPP_EXPOSED_AS(Delaunay2)
RCPP_EXPOSED_WRAP(Delaunay2)
RCPP_EXPOSED_AS(Delaunay3)
RCPP_EXPOSED_WRAP(Delaunay3)

RCPP_EXPOSED_AS(Regular2)
RCPP_EXPOSED_WRAP(Regular2)
RCPP_EXPOSED_AS(Regular3)
RCPP_EXPOSED_WRAP(Regular3)


#endif