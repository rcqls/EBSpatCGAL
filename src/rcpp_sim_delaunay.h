#include "rcpp_spatstat_triangulation.h"
#include "rcpp_sim.h"

typedef SimGibbs<Delaunay2,Point_2,Del2D_Vertex_handle> Del2SimGibbs2D;
typedef SimGibbs<Delaunay3,Point_3,Del3D_Vertex_handle> Del2SimGibbs3D;
