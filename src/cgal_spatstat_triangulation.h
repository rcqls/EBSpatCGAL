// The goal is to extend CGAL resources independently of Rcpp

#ifndef CGAL_SPATSTAT_TRIANGULATION_H
#define CGAL_SPATSTAT_TRIANGULATION_H

// CGAL headers
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <CGAL/point_generators_2.h>

#include <CGAL/Delaunay_triangulation_3.h>
#include <CGAL/Triangulation_vertex_base_3.h>
#include <CGAL/point_generators_3.h>

#include <CGAL/Regular_triangulation_euclidean_traits_2.h>
#include <CGAL/Regular_triangulation_2.h>

#include <CGAL/Regular_triangulation_3.h>
#include <CGAL/Regular_triangulation_euclidean_traits_3.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel 		K;
typedef K::Point_2 													Point_2;
typedef K::Circle_2 												Circle_2;
typedef K::Segment_2 												Segment_2;
typedef K::Ray_2 													Ray_2;
typedef CGAL::Delaunay_triangulation_2<K> 							Delaunay2;
//typedef K::Point_2 													Circle_2;
typedef double 														Weight;
typedef CGAL::Regular_triangulation_euclidean_traits_2<K,Weight>  	Gt2;
typedef CGAL::Regular_triangulation_2<Gt2> 							Regular2;
typedef Gt2::Weighted_point_2 										Weighted_point_2;
typedef CGAL::Delaunay_triangulation_3<K, CGAL::Fast_location> 		Delaunay3;
typedef K::Point_3 													Point_3;
typedef K::Segment_3												Segment_3;
typedef K::Ray_3													Ray_3;
typedef K::Triangle_3												Triangle_3;
typedef K::Tetrahedron_3 											Tetrahedron_3;
typedef K::Sphere_3 												Sphere_3;
typedef K::Object_3            										Object_3;
typedef CGAL::Regular_triangulation_euclidean_traits_3<K> 			Gt3;
typedef CGAL::Regular_triangulation_3<Gt3> 							Regular3;
typedef Gt3::Weighted_point_3 										Weighted_point_3;

// Our own declaration
//2D
typedef std::set<Delaunay2::Vertex_handle> Delaunay2_VertexSet; //edge or face or cell  vertices
typedef std::set<Delaunay2_VertexSet> Delaunay2_VertexSet_Set;
//3D
typedef std::set<Delaunay3::Vertex_handle> Delaunay3_VertexSet; //edge or face or cell  vertices
typedef std::set<Delaunay3_VertexSet> Delaunay3_VertexSet_Set;

std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> CGAL_Delaunay2_conflicted_and_boundary_edges(Delaunay2* obj, Point_2 p);
Delaunay2_VertexSet_Set CGAL_Delaunay2_incident_edges(Delaunay2* obj, Delaunay2::Vertex_handle v);
std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> CGAL_Delaunay3_conflicted_and_boundary_edges(Delaunay3* obj, Point_3 p);
Delaunay3_VertexSet_Set CGAL_Delaunay3_incident_edges(Delaunay3* obj, Delaunay3::Vertex_handle v);


#endif //CGAL_SPATSTAT_TRIANGULATION_H