#include "rcpp_term_delaunay.h"
#include "rcpp_dom.h"
#include "rcpp_sim.h"
#include "rcpp_lists_cache.h"

RCPP_EXPOSED_AS(Domain);
RCPP_EXPOSED_WRAP(Domain);

typedef ListsCache<Delaunay2> ListsCacheDel2D;
typedef ListsCache<Delaunay3> ListsCacheDel3D;

RCPP_EXPOSED_AS(ListsCacheDel2D);
RCPP_EXPOSED_WRAP(ListsCacheDel2D);
RCPP_EXPOSED_AS(ListsCacheDel3D);
RCPP_EXPOSED_WRAP(ListsCacheDel3D);

RCPP_EXPOSED_AS(Interaction);
RCPP_EXPOSED_WRAP(Interaction);



//inside number to determine here