#include "rcpp_term_delaunay.h"
#include "rcpp_dom.h"
#include "rcpp_sim.h"
#include "rcpp_lists_cache.h"

typedef Domain<Delaunay2> DomainDel2D;
typedef Domain<Delaunay3> DomainDel3D;

RCPP_EXPOSED_AS(DomainDel2D);
RCPP_EXPOSED_WRAP(DomainDel2D);
RCPP_EXPOSED_AS(DomainDel3D);
RCPP_EXPOSED_WRAP(DomainDel3D);

typedef SimGibbs<Delaunay2> SimGibbsDel2D;
typedef SimGibbs<Delaunay3> SimGibbsDel3D;

typedef ListsCache<Delaunay2> ListsCacheDel2D;
typedef ListsCache<Delaunay3> ListsCacheDel3D;

RCPP_EXPOSED_AS(ListsCacheDel2D);
RCPP_EXPOSED_WRAP(ListsCacheDel2D);
RCPP_EXPOSED_AS(ListsCacheDel3D);
RCPP_EXPOSED_WRAP(ListsCacheDel3D);

RCPP_EXPOSED_AS(Interaction);
RCPP_EXPOSED_WRAP(Interaction);



//inside number to determine here