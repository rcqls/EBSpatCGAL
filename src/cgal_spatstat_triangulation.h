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
typedef std::set<Delaunay3::Vertex_handle> Delaunay3_VertexSet; //edge or face or cell  vertices
typedef std::set<Delaunay3_VertexSet> Delaunay3_VertexSet_Set;

// Delaunay 3D edges components: all needed for edges when inserting a point

std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> CGAL_Delaunay3_conflicted_and_boundary_edges(Delaunay3* obj, Point_3 p) {

 	// Locate the point
    Delaunay3::Locate_type lt;
    int li, lj;
    Delaunay3::Cell_handle c = obj->locate(p, lt, li, lj);
    
    //Is it possible? And if yes what to return? 
    //=> if (lt == Delaunay3::VERTEX) return NA_REAL;

    // Get the cells that conflict with p in a vector V,
    // and a facet on the boundary of this hole in f.
    std::vector<Delaunay3::Cell_handle> V;
    std::vector<Delaunay3::Facet> f;


	obj->find_conflicts(p, c,
                     std::back_inserter(f), // Get one boundary facet
                     std::back_inserter(V));// Conflict cells in V

	int i=0;  

	Delaunay3_VertexSet_Set boundaryEdges;

	for(std::vector<Delaunay3::Facet>::iterator fit = f.begin();
       fit != f.end();
       ++fit,++i){

		Delaunay3::Vertex_handle v0=((*fit).first->vertex(((*fit).second+1)%4)),v1=((*fit).first->vertex(((*fit).second+2)%4)),v2=((*fit).first->vertex(((*fit).second+3)%4));
		
		Delaunay3_VertexSet e0;
		e0.insert(v0);e0.insert(v1);boundaryEdges.insert(e0);
		Delaunay3_VertexSet e1;
		e1.insert(v0);e1.insert(v2);boundaryEdges.insert(e1);
		Delaunay3_VertexSet e2;
		e2.insert(v1);e2.insert(v2);boundaryEdges.insert(e2);
	}

	std::cout << "Number of boundary edges=" << 3*i << " (amoung " << f.size() << " facets)" << std::endl;

	Delaunay3_VertexSet_Set conflictedEdges;

	i=0;
	for(std::vector<Delaunay3::Cell_handle>::iterator cit = V.begin();
       cit != V.end();
       ++cit,++i){
		Delaunay3::Vertex_handle v0=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(0),v1=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(1),v2=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(2),v3=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(3);
		Delaunay3_VertexSet e0;
		e0.insert(v0);e0.insert(v1);
		if(boundaryEdges.find(e0) == boundaryEdges.end()) conflictedEdges.insert(e0);
		Delaunay3_VertexSet e1;
		e1.insert(v0);e1.insert(v2);
		if(boundaryEdges.find(e1) == boundaryEdges.end()) conflictedEdges.insert(e1);
		Delaunay3_VertexSet e2;
		e2.insert(v0);e2.insert(v3);
		if(boundaryEdges.find(e2) == boundaryEdges.end()) conflictedEdges.insert(e2);
		Delaunay3_VertexSet e3;
		e3.insert(v1);e3.insert(v2);
		if(boundaryEdges.find(e3) == boundaryEdges.end()) conflictedEdges.insert(e3);
		Delaunay3_VertexSet e4;
		e4.insert(v1);e4.insert(v3);
		if(boundaryEdges.find(e4) == boundaryEdges.end()) conflictedEdges.insert(e4);
		Delaunay3_VertexSet e5;
		e5.insert(v2);e5.insert(v3);
		if(boundaryEdges.find(e5) == boundaryEdges.end()) conflictedEdges.insert(e5);

	}

	std::cout << "Number of conflicted edges=" << 6*i << " (amoung " << V.size() << " cells)" << std::endl;

	std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> ret;
	ret = std::make_pair(boundaryEdges,conflictedEdges);
	return ret;
}  


#endif //CGAL_SPATSTAT_TRIANGULATION_H