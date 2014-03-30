#include "rcpp_spatstat_triangulation.h"
#include "rcpp_sim.h"

typedef Domain<Delaunay2> DomainDel2D;
typedef Domain<Delaunay3> DomainDel3D;

RCPP_EXPOSED_AS(DomainDel2D);
RCPP_EXPOSED_WRAP(DomainDel2D);
RCPP_EXPOSED_AS(DomainDel3D);
RCPP_EXPOSED_WRAP(DomainDel3D);

typedef SimGibbs<Delaunay2> SimGibbsDel2D;
typedef SimGibbs<Delaunay3> SimGibbsDel3D;

//inside number to determine here