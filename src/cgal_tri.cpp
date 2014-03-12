#include <Rcpp.h>
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

using namespace Rcpp;

// helpers
IntegerVector Delaunay2_insert( Delaunay2* obj, NumericVector ptsX, NumericVector ptsY ) {
	std::vector<Point_2> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
    	points.push_back(Point_2(ptsX[i],ptsY[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
	
	return IntegerVector::create(obj->number_of_vertices());
}

void Delaunay3_insert( Delaunay3* obj, NumericVector ptsX, NumericVector ptsY, NumericVector ptsZ ) {
	std::vector<Point_3> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
    	points.push_back(Point_3(ptsX[i],ptsY[i],ptsZ[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
}

void Regular2_insert( Regular2* obj, NumericVector ptsX, NumericVector ptsY,  NumericVector ptsW) {
	std::vector<Point_2> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
    	points.push_back(Weighted_point_2(Point_2(ptsX[i],ptsY[i]),ptsW[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
}

void Regular3_insert( Regular3* obj, NumericVector ptsX, NumericVector ptsY, NumericVector ptsZ,  NumericVector ptsW) {
	std::vector<Weighted_point_3> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
		points.push_back(Weighted_point_3(Point_3(ptsX[i],ptsY[i],ptsZ[i]),ptsW[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
}

template <typename TRIANGULATION>
void Triangulation_remove_at_pos( TRIANGULATION* obj, IntegerVector rank) {

	int n=rank[0]-1;

	//std::cout << "pos of point to remove: " << n << std::endl;


	if(n >=0 && n < obj->number_of_vertices()) {
		typename TRIANGULATION::Finite_vertices_iterator vit=obj->finite_vertices_begin();
		for(int i=0;i<n;++i) ++vit;
		obj->remove(vit);
	}

}


void Delaunay2_remove_neighbour_of( Delaunay2* obj, NumericVector xy) {

	Point_2 pt(xy[0],xy[1]);

	Delaunay2::Vertex_handle selected_vertex = obj->nearest_vertex(pt);

	obj->remove(selected_vertex);

}

void Delaunay3_remove_neighbour_of( Delaunay3* obj, NumericVector xyz) {

	Point_3 pt(xyz[0],xyz[1],xyz[2]);

	Delaunay3::Vertex_handle selected_vertex = obj->nearest_vertex(pt);

	obj->remove(selected_vertex);

}

template <typename TRIANGULATION2>
NumericMatrix Triangulation2_vertices(TRIANGULATION2* obj) {

	NumericMatrix dv(obj->number_of_vertices(),2);
	int i=0;
	for(typename TRIANGULATION2::Finite_vertices_iterator 
          vit = obj->finite_vertices_begin(),
          end = obj->finite_vertices_end();
        vit!= end; ++vit,++i)
    {
       dv(i,0)=vit->point().x();dv(i,1)=vit->point().y();
    }
    return dv;
}

template <typename TRIANGULATION3>
NumericMatrix Triangulation3_vertices(TRIANGULATION3* obj) {

	NumericMatrix dv(obj->number_of_vertices(),3);
	int i=0;
	for(typename TRIANGULATION3::Finite_vertices_iterator 
          vit = obj->finite_vertices_begin(),
          end = obj->finite_vertices_end();
        vit!= end; ++vit,++i)
    {
       dv(i,0)=vit->point().x();dv(i,1)=vit->point().y();dv(i,2)=vit->point().z();
    }
    return dv;
}

template <typename TRIANGULATION2>
NumericMatrix Triangulation2_edges(TRIANGULATION2* obj) {


	int i=0,nb=iterator_distance(obj->finite_edges_begin(),obj->finite_edges_end()); 

	NumericMatrix de(nb,4);

	Segment_2 segment;

	for(typename TRIANGULATION2::Finite_edges_iterator eit = obj->finite_edges_begin();
        eit != obj->finite_edges_end();
        ++eit,++i){
      	segment=obj->segment(*eit);
      	de(i,0)=segment.source().x();
      	de(i,1)=segment.source().y();
      	de(i,2)=segment.target().x();
      	de(i,3)=segment.target().y();
    }

    return de;
}

template <typename TRIANGULATION3>
NumericMatrix Triangulation3_edges(TRIANGULATION3* obj) {


	int i=0,nb=iterator_distance(obj->finite_edges_begin(),obj->finite_edges_end()); 
        
	NumericMatrix de(nb,6);

	Segment_3 segment;

	for(typename TRIANGULATION3::Finite_edges_iterator eit = obj->finite_edges_begin();
        eit != obj->finite_edges_end();
        ++eit,++i){
      	segment=obj->segment(*eit);
      	de(i,0)=segment.source().x();
      	de(i,1)=segment.source().y();
      	de(i,2)=segment.source().z();
      	de(i,3)=segment.target().x();
      	de(i,4)=segment.target().y();
      	de(i,5)=segment.target().z();
    }

    return de;
}

template <typename TRIANGULATION3>
NumericMatrix Triangulation3_facets(TRIANGULATION3* obj) {


	int i=0,nb=iterator_distance(obj->finite_facets_begin(),obj->finite_facets_end()); 

	NumericMatrix df(nb,9);

	Triangle_3 triangle;

	for(typename TRIANGULATION3::Finite_facets_iterator eit = obj->finite_facets_begin();
        eit != obj->finite_facets_end();
        ++eit,++i){
      	triangle=obj->triangle(*eit);
      	df(i,0)=triangle.vertex(0).x();
      	df(i,1)=triangle.vertex(0).y();
      	df(i,2)=triangle.vertex(0).z();
      	df(i,3)=triangle.vertex(1).x();
      	df(i,4)=triangle.vertex(1).y();
      	df(i,5)=triangle.vertex(1).z();
      	df(i,6)=triangle.vertex(2).x();
      	df(i,7)=triangle.vertex(2).y();
      	df(i,8)=triangle.vertex(2).z();
    }

    return df;
}

template <typename TRIANGULATION3>
NumericMatrix Triangulation3_cells(TRIANGULATION3* obj) {


	int i=0,nb=iterator_distance(obj->finite_cells_begin(),obj->finite_cells_end()); 

	NumericMatrix dc(nb,12);

	Tetrahedron_3 tetra;

	for(typename TRIANGULATION3::Finite_cells_iterator eit = obj->finite_cells_begin();
        eit != obj->finite_cells_end();
        ++eit,++i){
      	tetra=obj->tetrahedron(eit);
      	dc(i,0)=tetra.vertex(0).x();
      	dc(i,1)=tetra.vertex(0).y();
      	dc(i,2)=tetra.vertex(0).z();
      	dc(i,3)=tetra.vertex(1).x();
      	dc(i,4)=tetra.vertex(1).y();
      	dc(i,5)=tetra.vertex(1).z();
      	dc(i,6)=tetra.vertex(2).x();
      	dc(i,7)=tetra.vertex(2).y();
      	dc(i,8)=tetra.vertex(2).z();
      	dc(i,9)=tetra.vertex(3).x();
      	dc(i,10)=tetra.vertex(3).y();
      	dc(i,11)=tetra.vertex(3).z();
    }

    return dc;
}



/*void Delaunay_show_vertices(Delaunay* obj) {

	for(Delaunay::Finite_vertices_iterator 
          vit = obj->finite_vertices_begin(),
          end = obj->finite_vertices_end();
        vit!= end; ++vit)
    {
      std::cout << vit->point() << std::endl;
    }
}*/


template <typename TRIANGULATION2>
NumericMatrix Triangulation2_dual_vertices(TRIANGULATION2* obj) {
	int i=0,nb=iterator_distance(obj->finite_faces_begin(),obj->finite_faces_end());

	NumericMatrix vv(nb,3);

	for(typename TRIANGULATION2::Finite_faces_iterator fit = obj->finite_faces_begin();
        fit != obj->finite_faces_end(); ++fit,++i) {
		Point_2 v0=obj->triangle(fit).vertex(0),v1=obj->triangle(fit).vertex(1),v2=obj->triangle(fit).vertex(2);
	    Circle_2 c =  Circle_2(v0,v1,v2);
	    Point_2 p=c.center();

    	vv(i,0)=p.x();
     	vv(i,1)=p.y();
     	vv(i,2)=c.squared_radius();

    }

    return vv;
}

template <typename TRIANGULATION3>
NumericMatrix Triangulation3_dual_vertices(TRIANGULATION3* obj) {
	int i=0,nb=iterator_distance(obj->finite_cells_begin(),obj->finite_cells_end());

	NumericMatrix vv(nb,4);

	for(typename TRIANGULATION3::Finite_cells_iterator fit = obj->finite_cells_begin();
        fit != obj->finite_cells_end(); ++fit,++i) {
		Point_3 v0=obj->tetrahedron(fit).vertex(0),v1=obj->tetrahedron(fit).vertex(1),v2=obj->tetrahedron(fit).vertex(2),v3=obj->tetrahedron(fit).vertex(3);
	    Sphere_3 c =  Sphere_3(v0,v1,v2,v3);
	    Point_3 p=c.center();

    	vv(i,0)=p.x();
     	vv(i,1)=p.y();
     	vv(i,2)=p.z();
     	vv(i,3)=c.squared_radius();

    }

    return vv;
}

template <typename TRIANGULATION2>
NumericMatrix Triangulation2_dual_edges(TRIANGULATION2* obj) {
	int i=0,nb=iterator_distance(obj->finite_edges_begin(),obj->finite_edges_end()); 
	Point_2 p1,p2;

	NumericMatrix ve(nb,4);

	for(typename TRIANGULATION2::Finite_edges_iterator fit = obj->finite_edges_begin();
        fit != obj->finite_edges_end(); ++fit,++i) {
	    Object_3 o = obj->dual(*fit);

	    if (const Segment_2 *s = CGAL::object_cast<Segment_2>(&o)) {
	          p1=s->vertex(0); p2=s->vertex(1);
	    } else if (const Ray_2 *r = CGAL::object_cast<Ray_2>(&o)) {
	         p1=r->point(0);p2=r->point(1);   
	    }

    	ve(i,0)=p1.x();
     	ve(i,1)=p1.y();
     	ve(i,2)=p2.x();
     	ve(i,3)=p2.y();

    }//end-for-edges

    return ve;
}

template <typename TRIANGULATION3>
NumericMatrix Triangulation3_dual_edges(TRIANGULATION3* obj) {
	int i=0,nb=iterator_distance(obj->finite_facets_begin(),obj->finite_facets_end()); 
	Point_3 p1,p2;

	NumericMatrix ve(nb,6);

	for(typename TRIANGULATION3::Finite_facets_iterator fit = obj->finite_facets_begin();
        fit != obj->finite_facets_end(); ++fit,++i) {
	    Object_3 o = obj->dual(*fit);

	    if (const Segment_3 *s = CGAL::object_cast<Segment_3>(&o)) {
	          p1=s->vertex(0); p2=s->vertex(1);
	    } else if (const Ray_3 *r = CGAL::object_cast<Ray_3>(&o)) {
	         p1=r->point(0);p2=r->point(1);   
	    }

    	ve(i,0)=p1.x();
     	ve(i,1)=p1.y();
     	ve(i,2)=p1.z();
     	ve(i,3)=p2.x();
     	ve(i,4)=p2.y();
     	ve(i,5)=p2.z();

    }//end-for-edges

    return ve;
}

NumericMatrix Delaunay2_conflicted_faces(Delaunay2* obj, NumericVector xy) {
	std::list<Delaunay2::Face_handle> faces;
 	Point_2 p(xy[0],xy[1]);
 	obj->get_conflicts (p, std::back_inserter(faces));

 	int i=0,nb=iterator_distance(faces.begin(),faces.end()); 

 	NumericMatrix fc(nb,6);

	for(std::list<Delaunay2::Face_handle>::iterator fit = faces.begin();
      fit != faces.end();
      ++fit,++i){
      	if(obj->is_infinite(*fit)) {
      		std::cout << i << " infinite face" << std::endl;
      	}
		Delaunay2::Vertex_handle v0=static_cast<Delaunay2::Face_handle>(*fit)->vertex(0),v1=static_cast<Delaunay2::Face_handle>(*fit)->vertex(1),v2=static_cast<Delaunay2::Face_handle>(*fit)->vertex(2);
		Point_2 p0=v0->point(),p1=v1->point(),p2=v2->point();
		if(obj->is_infinite(v0)) {
			fc(i,0)=NA_REAL;
     		fc(i,1)=NA_REAL;
		} else {
    		fc(i,0)=p0.x();
     		fc(i,1)=p0.y();
     	}
     	if(obj->is_infinite(v1)) {
			fc(i,2)=NA_REAL;
     		fc(i,3)=NA_REAL;
		} else {
    		fc(i,2)=p1.x();
     		fc(i,3)=p1.y();
     	}
     	if(obj->is_infinite(v2)) {
			fc(i,4)=NA_REAL;
     		fc(i,5)=NA_REAL;
		} else {
    		fc(i,4)=p2.x();
     		fc(i,5)=p2.y();
     	}
	}
	return fc;
}

NumericMatrix Delaunay3_conflicted_cells(Delaunay3* obj, NumericVector xy) {
 	Point_3 p(xy[0],xy[1],xy[2]);

 	// Locate the point
    Delaunay3::Locate_type lt;
    int li, lj;
    Delaunay3::Cell_handle c = obj->locate(p, lt, li, lj);
    if (lt == Delaunay3::VERTEX) return NA_REAL;
       

    // Get the cells that conflict with p in a vector V,
    // and a facet on the boundary of this hole in f.
    std::vector<Delaunay3::Cell_handle> V;
    std::vector<Delaunay3::Facet> f;


	obj->find_conflicts(p, c,
                     std::back_inserter(f), // Get one boundary facet
                     std::back_inserter(V));          // Conflict cells in V

	int i=0; //,nb=iterator_distance(V.begin(),V.end()); 

 	NumericMatrix fc(V.size(),12);

    // if ((V.size() & 1) == 0)  // Even number of conflict cells ?
    //   T.insert_in_hole(p, V.begin(), V.end(), f.first, f.second);

	for(std::vector<Delaunay3::Cell_handle>::iterator cit = V.begin();
       cit != V.end();
       ++cit,++i){
 //      	if(obj->is_infinite(*fit)) {
 //      		std::cout << i << " infinite face" << std::endl;
 //      	}
	Delaunay3::Vertex_handle v0=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(0),v1=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(1),v2=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(2),v3=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(3);
	Point_3 p0=v0->point(),p1=v1->point(),p2=v2->point(),p3=v3->point();
		if(obj->is_infinite(v0)) {
			fc(i,0)=NA_REAL;
     		fc(i,1)=NA_REAL;
     		fc(i,2)=NA_REAL;
		} else {
    		fc(i,0)=p0.x();
     		fc(i,1)=p0.y();
     		fc(i,2)=p0.z();
     	}
     	if(obj->is_infinite(v1)) {
			fc(i,3)=NA_REAL;
     		fc(i,4)=NA_REAL;
     		fc(i,5)=NA_REAL;
		} else {
    		fc(i,3)=p1.x();
     		fc(i,4)=p1.y();
     		fc(i,5)=p1.z();
     	}
     	if(obj->is_infinite(v2)) {
			fc(i,6)=NA_REAL;
     		fc(i,7)=NA_REAL;
     		fc(i,8)=NA_REAL;
		} else {
    		fc(i,6)=p2.x();
     		fc(i,7)=p2.y();
     		fc(i,8)=p2.z();
     	}
     	if(obj->is_infinite(v3)) {
			fc(i,9)=NA_REAL;
     		fc(i,10)=NA_REAL;
     		fc(i,11)=NA_REAL;
		} else {
    		fc(i,9)=p3.x();
     		fc(i,10)=p3.y();
     		fc(i,11)=p3.z();
     	}
     	 
	}
	return fc;
}

List Delaunay3_conflicted_cells_and_boundary_facets(Delaunay3* obj, NumericVector xy) {
 	Point_3 p(xy[0],xy[1],xy[2]);

 	// Locate the point
    Delaunay3::Locate_type lt;
    int li, lj;
    Delaunay3::Cell_handle c = obj->locate(p, lt, li, lj);
    if (lt == Delaunay3::VERTEX) return NA_REAL;
       

    // Get the cells that conflict with p in a vector V,
    // and a facet on the boundary of this hole in f.
    std::vector<Delaunay3::Cell_handle> V;
    std::vector<Delaunay3::Facet> f;


	obj->find_conflicts(p, c,
                     std::back_inserter(f), // Get one boundary facet
                     std::back_inserter(V));          // Conflict cells in V

	int i=0; //,nb=iterator_distance(V.begin(),V.end()); 

 	NumericMatrix cc(V.size(),12);

    // if ((V.size() & 1) == 0)  // Even number of conflict cells ?
    //   T.insert_in_hole(p, V.begin(), V.end(), f.first, f.second);

	for(std::vector<Delaunay3::Cell_handle>::iterator cit = V.begin();
       cit != V.end();
       ++cit,++i){
 //      	if(obj->is_infinite(*fit)) {
 //      		std::cout << i << " infinite face" << std::endl;
 //      	}
	Delaunay3::Vertex_handle v0=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(0),v1=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(1),v2=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(2),v3=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(3);
	Point_3 p0=v0->point(),p1=v1->point(),p2=v2->point(),p3=v3->point();
		if(obj->is_infinite(v0)) {
			cc(i,0)=NA_REAL;
     		cc(i,1)=NA_REAL;
     		cc(i,2)=NA_REAL;
		} else {
    		cc(i,0)=p0.x();
     		cc(i,1)=p0.y();
     		cc(i,2)=p0.z();
     	}
     	if(obj->is_infinite(v1)) {
			cc(i,3)=NA_REAL;
     		cc(i,4)=NA_REAL;
     		cc(i,5)=NA_REAL;
		} else {
    		cc(i,3)=p1.x();
     		cc(i,4)=p1.y();
     		cc(i,5)=p1.z();
     	}
     	if(obj->is_infinite(v2)) {
			cc(i,6)=NA_REAL;
     		cc(i,7)=NA_REAL;
     		cc(i,8)=NA_REAL;
		} else {
    		cc(i,6)=p2.x();
     		cc(i,7)=p2.y();
     		cc(i,8)=p2.z();
     	}
     	if(obj->is_infinite(v3)) {
			cc(i,9)=NA_REAL;
     		cc(i,10)=NA_REAL;
     		cc(i,11)=NA_REAL;
		} else {
    		cc(i,9)=p3.x();
     		cc(i,10)=p3.y();
     		cc(i,11)=p3.z();
     	}
	}

	NumericMatrix fc(f.size(),9);

    // if ((V.size() & 1) == 0)  // Even number of conflict cells ?
    //   T.insert_in_hole(p, V.begin(), V.end(), f.first, f.second);

	i=0;
	for(std::vector<Delaunay3::Facet>::iterator fit = f.begin();
       fit != f.end();
       ++fit,++i){
 //      	if(obj->is_infinite(*fit)) {
 //      		std::cout << i << " infinite face" << std::endl;
 //      	}

		Delaunay3::Vertex_handle v0=((*fit).first->vertex(((*fit).second+1)&3)),v1=((*fit).first->vertex(((*fit).second+2)&3)),v2=((*fit).first->vertex(((*fit).second+3)&3));
		Point_3 p0=v0->point(),p1=v1->point(),p2=v2->point();
		fc(i,0)=p0.x();
 		fc(i,1)=p0.y();
 		fc(i,2)=p0.z();

		fc(i,3)=p1.x();
 		fc(i,4)=p1.y();
 		fc(i,5)=p1.z();
 	
		fc(i,6)=p2.x();
 		fc(i,7)=p2.y();
 		fc(i,8)=p2.z();
	}

	List ret; ret["cells"] = cc; ret["boundaryFacets"] = fc;
	return ret;
}

List Delaunay3_conflicted_edges_and_boundary_edges(Delaunay3* obj, NumericVector xy) {
 	Point_3 p(xy[0],xy[1],xy[2]);

 	// Locate the point
    Delaunay3::Locate_type lt;
    int li, lj;
    Delaunay3::Cell_handle c = obj->locate(p, lt, li, lj);
    if (lt == Delaunay3::VERTEX) return NA_REAL;
    typedef std::set<Point_3> Point_3_Set;

    // Get the cells that conflict with p in a vector V,
    // and a facet on the boundary of this hole in f.
    std::vector<Delaunay3::Cell_handle> V;
    std::vector<Delaunay3::Facet> f;


	obj->find_conflicts(p, c,
                     std::back_inserter(f), // Get one boundary facet
                     std::back_inserter(V));          // Conflict cells in V

	int i=0;  

	std::set<Point_3_Set> boundaryEdges;

	for(std::vector<Delaunay3::Facet>::iterator fit = f.begin();
       fit != f.end();
       ++fit,++i){

		Delaunay3::Vertex_handle v0=((*fit).first->vertex(((*fit).second+1)&3)),v1=((*fit).first->vertex(((*fit).second+2)&3)),v2=((*fit).first->vertex(((*fit).second+3)&3));
		Point_3 p0=v0->point(),p1=v1->point(),p2=v2->point();
		std::set<Point_3> e0;
		e0.insert(p0);e0.insert(p1);boundaryEdges.insert(e0);
		std::set<Point_3> e1;
		e1.insert(p0);e1.insert(p2);boundaryEdges.insert(e1);
		std::set<Point_3> e2;
		e2.insert(p1);e2.insert(p2);boundaryEdges.insert(e2);
	}

	std::set<Point_3_Set> conflictedEdges;

	i=0;
	for(std::vector<Delaunay3::Cell_handle>::iterator cit = V.begin();
       cit != V.end();
       ++cit,++i){
		Delaunay3::Vertex_handle v0=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(0),v1=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(1),v2=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(2),v3=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(3);
		Point_3 p0=v0->point(),p1=v1->point(),p2=v2->point(),p3=v3->point();
		std::set<Point_3> e0;
		e0.insert(p0);e0.insert(p1);
		//if(conflictedEdges.find(e0) != conflictedEdges.end()) 
		conflictedEdges.insert(e0);
		std::set<Point_3> e1;
		e1.insert(p0);e1.insert(p2);
		//if(conflictedEdges.find(e1) != conflictedEdges.end()) 
		conflictedEdges.insert(e1);
		std::set<Point_3> e2;
		e2.insert(p0);e2.insert(p3);
		//if(conflictedEdges.find(e2) != conflictedEdges.end()) 
		conflictedEdges.insert(e2);
		std::set<Point_3> e3;
		e3.insert(p1);e3.insert(p2);
		//if(conflictedEdges.find(e3) != conflictedEdges.end()) 
		conflictedEdges.insert(e3);
		std::set<Point_3> e4;
		e4.insert(p1);e4.insert(p3);
		//if(conflictedEdges.find(e4) != conflictedEdges.end()) 
		conflictedEdges.insert(e4);
		std::set<Point_3> e5;
		e5.insert(p2);e5.insert(p3);
		//if(conflictedEdges.find(e5) != conflictedEdges.end()) 
		conflictedEdges.insert(e5);

		// if(obj->is_infinite(v0)) {
		// 	cc(i,0)=NA_REAL;
  //    		cc(i,1)=NA_REAL;
  //    		cc(i,2)=NA_REAL;
		// } else {
  //   		cc(i,0)=p0.x();
  //    		cc(i,1)=p0.y();
  //    		cc(i,2)=p0.z();
  //    	}
  //    	if(obj->is_infinite(v1)) {
		// 	cc(i,3)=NA_REAL;
  //    		cc(i,4)=NA_REAL;
  //    		cc(i,5)=NA_REAL;
		// } else {
  //   		cc(i,3)=p1.x();
  //    		cc(i,4)=p1.y();
  //    		cc(i,5)=p1.z();
  //    	}
  //    	if(obj->is_infinite(v2)) {
		// 	cc(i,6)=NA_REAL;
  //    		cc(i,7)=NA_REAL;
  //    		cc(i,8)=NA_REAL;
		// } else {
  //   		cc(i,6)=p2.x();
  //    		cc(i,7)=p2.y();
  //    		cc(i,8)=p2.z();
  //    	}
  //    	if(obj->is_infinite(v3)) {
		// 	cc(i,9)=NA_REAL;
  //    		cc(i,10)=NA_REAL;
  //    		cc(i,11)=NA_REAL;
		// } else {
  //   		cc(i,9)=p3.x();
  //    		cc(i,10)=p3.y();
  //    		cc(i,11)=p3.z();
  //    	}
	}

    // if ((V.size() & 1) == 0)  // Even number of conflict cells ?
    //   T.insert_in_hole(p, V.begin(), V.end(), f.first, f.second);

	

	NumericMatrix be(boundaryEdges.size(),6);
	NumericMatrix ce(conflictedEdges.size(),6);

	i=0;
	for(std::set<Point_3_Set>::iterator eit = boundaryEdges.begin();
       eit != boundaryEdges.end();
       ++eit,++i) {
		be(i,0)=(*((*eit).begin())).x();
 		be(i,1)=(*((*eit).begin())).y();
 		be(i,2)=(*((*eit).begin())).z();
 		be(i,3)=(*((*eit).rbegin())).x();
 		be(i,4)=(*((*eit).rbegin())).y();
 		be(i,5)=(*((*eit).rbegin())).z();

 	}

 	i=0;
 	for(std::set<Point_3_Set>::iterator eit = conflictedEdges.begin();
       eit != conflictedEdges.end();
       ++eit,++i){
		ce(i,0)=(*((*eit).begin())).x();
 		ce(i,1)=(*((*eit).begin())).y();
 		ce(i,2)=(*((*eit).begin())).z();
 		ce(i,3)=(*((*eit).rbegin())).x();
 		ce(i,4)=(*((*eit).rbegin())).y();
 		ce(i,5)=(*((*eit).rbegin())).z();
 	}

	List ret; ret["conflicted_edges"] = ce; ret["boundary_edges"] = be;
	return ret;
}

//same as above with circles (center + radius)
NumericMatrix Delaunay2_conflicted_faces_with_circles(Delaunay2* obj, NumericVector xy) {
	std::list<Delaunay2::Face_handle> faces;
 	Point_2 p(xy[0],xy[1]);
 	obj->find_conflicts (p, faces);

 	int i=0,nb=iterator_distance(faces.begin(),faces.end()); 

 	NumericMatrix fc(nb,9);

	for(std::list<Delaunay2::Face_handle>::iterator fit = faces.begin();
      fit != faces.end();
      ++fit,++i){
      	if(obj->is_infinite(*fit)) {
      		std::cout << i << " infinite face" << std::endl;
      	}
		Delaunay2::Vertex_handle v0=static_cast<Delaunay2::Face_handle>(*fit)->vertex(0),v1=static_cast<Delaunay2::Face_handle>(*fit)->vertex(1),v2=static_cast<Delaunay2::Face_handle>(*fit)->vertex(2);
		Point_2 p0=v0->point(),p1=v1->point(),p2=v2->point();
	    Circle_2 circ =  Circle_2(p0,p1,p2);
	    Point_2 c=circ.center();
	    int ok=0;
		if(obj->is_infinite(v0)) {
			fc(i,0)=NA_REAL;
     		fc(i,1)=NA_REAL;
     		ok += 1;
		} else {
    		fc(i,0)=p0.x();
     		fc(i,1)=p0.y();
     	}
     	if(obj->is_infinite(v1)) {
			fc(i,2)=NA_REAL;
     		fc(i,3)=NA_REAL;
     		ok += 1;
		} else {
    		fc(i,2)=p1.x();
     		fc(i,3)=p1.y();
     	}
     	if(obj->is_infinite(v2)) {
			fc(i,4)=NA_REAL;
     		fc(i,5)=NA_REAL;
     		ok += 1;
		} else {
    		fc(i,4)=p2.x();
     		fc(i,5)=p2.y();
     	}
     	if(ok>0) {
			fc(i,6)=NA_REAL;
	     	fc(i,7)=NA_REAL;
	     	fc(i,8)=NA_REAL;
     	} else {
	     	fc(i,6)=c.x();
	     	fc(i,7)=c.y();
	     	fc(i,8)=circ.squared_radius();
	    }
	}
	return fc;
}



template <typename TRIANGULATION2>
NumericMatrix Triangulation2_incident_vertices(TRIANGULATION2* obj, IntegerVector rank) {
	int n=rank[0]-1;

	//std::cout << "pos of point to remove: " << n << std::endl;


	if(n >=0 && n < obj->number_of_vertices()) {
		typename TRIANGULATION2::Finite_vertices_iterator vit=obj->finite_vertices_begin();
		for(int i=0;i<n;++i) ++vit;
		typename TRIANGULATION2::Vertex_circulator vc=obj->incident_vertices(vit),done(vc);
  		if (vc != 0) {
  			int i=0,nb=circulator_size(vc); 
 			NumericMatrix vi(nb,2);
	    	do {
		      	vi(i,0)=vc->point().x();
		      	vi(i,1)=vc->point().y();
				++i;
	    	} while(++vc != done);
	    	return vi;
  		}
	}
	NumericMatrix vi(0,0);
	return vi;

}


template <typename TRIANGULATION2>
NumericMatrix Triangulation2_incident_edges(TRIANGULATION2* obj, IntegerVector rank) {
	int n=rank[0]-1;

	//std::cout << "pos of point to remove: " << n << std::endl;


	if(n >=0 && n < obj->number_of_vertices()) {
		typename TRIANGULATION2::Finite_vertices_iterator vit=obj->finite_vertices_begin();
		for(int i=0;i<n;++i) ++vit;
		typename TRIANGULATION2::Edge_circulator ec=obj->incident_edges(vit),done(ec);
  		if (ec != 0) {
  			int i=0,nb=circulator_size(ec); 
 			NumericMatrix ei(nb,4);
 			Segment_2 segment;
	    	do {
	    		
	    		segment=obj->segment(*ec);
		      	ei(i,0)=segment.source().x();
		      	ei(i,1)=segment.source().y();
		      	ei(i,2)=segment.target().x();
		      	ei(i,3)=segment.target().y();
				++i;
	    	} while(++ec != done);
	    	return ei;
  		}
	}
	NumericMatrix ei(0,0);
	return ei;

}

template <typename TRIANGULATION2>
NumericMatrix Triangulation2_incident_faces(TRIANGULATION2* obj, IntegerVector rank) {
	int n=rank[0]-1;

	//std::cout << "pos of point to remove: " << n << std::endl;


	if(n >=0 && n < obj->number_of_vertices()) {
		typename TRIANGULATION2::Finite_vertices_iterator vit=obj->finite_vertices_begin();
		for(int i=0;i<n;++i) ++vit;
		typename TRIANGULATION2::Face_circulator fc=obj->incident_faces(vit),done(fc);
  		if (fc != 0) {
  			int i=0,nb=circulator_size(fc); 
 			NumericMatrix fi(nb,9);
	    	do {
	    		typename TRIANGULATION2::Face_handle fh=static_cast<typename TRIANGULATION2::Face_handle>(fc);
	    		Point_2 p0=fh->vertex(0)->point(),p1=fh->vertex(1)->point(),p2=fh->vertex(2)->point(),
	    			vv=obj->circumcenter(fh);
	    		fi(i,0)=p0.x();fi(i,1)=p0.y();
	    		fi(i,2)=p1.x();fi(i,3)=p1.y();
	    		fi(i,4)=p2.x();fi(i,5)=p2.y();
	    		fi(i,6)=vv.x();fi(i,7)=vv.y();
	    		fi(i,8)=(p0.x()-vv.x())*(p0.x()-vv.x())+(p0.y()-vv.y())*(p0.y()-vv.y());
				++i;
	    	} while(++fc != done);
	    	return fi;
  		}
	}
	NumericMatrix fi(0,0);
	return fi;

}

//TO detect if an external pointer is nil
SEXP is_xptr_null(SEXP objR) {
	void *objPtr;
	bool ans;
	objPtr=R_ExternalPtrAddr(objR);
  	if(objPtr==NULL) {
  		ans=1;
	} else {
		ans=0;
	}
	return Rcpp::wrap(ans);
}


RCPP_MODULE(cgal_module) {
	function( "is_xptr_null", &is_xptr_null );
	class_<Delaunay2>( "Delaunay2" )
	.constructor()
	.method("insert",&Delaunay2_insert,"insert points")
	.method("remove_at_pos",&Triangulation_remove_at_pos<Delaunay2>,"remove point at position")
	.method("remove_neighbour_of",&Delaunay2_remove_neighbour_of,"remove point neighbour of point")
	.method("vertices",&Triangulation2_vertices<Delaunay2>,"vertices coordinates")
	.method("edges",&Triangulation2_edges<Delaunay2>,"edges coordinates")
	.method("dual_vertices",&Triangulation2_dual_vertices<Delaunay2>,"dual vertices coordinates")
	.method("dual_edges",&Triangulation2_dual_edges<Delaunay2>,"dual edges coordinates")
	.method("conflicted_faces",&Delaunay2_conflicted_faces,"conflicted faces")
	.method("conflicted_faces_with_circles",&Delaunay2_conflicted_faces_with_circles,"conflicted faces with circles")
	.method("incident_vertices",&Triangulation2_incident_vertices<Delaunay2>,"incident edges")
	.method("incident_edges",&Triangulation2_incident_edges<Delaunay2>,"incident edges")
	.method("incident_faces",&Triangulation2_incident_faces<Delaunay2>,"incident faces")
	//.method("show_vertices",&Delaunay_show_vertices,"vertices coordinates")
	;
	class_<Regular2>( "Regular2" )
	.constructor()
	.method("insert",&Regular2_insert,"insert points")
	.method("remove_at_pos",&Triangulation_remove_at_pos<Regular2>,"remove point at position")
	//.method("remove_neighbour_of",&Regular_remove_neighbour_of<Regular>,"remove point neighbour of point")
	.method("vertices",&Triangulation2_vertices<Regular2>,"vertices coordinates")
	.method("edges",&Triangulation2_edges<Regular2>,"edges coordinates")
	.method("dual_edges",&Triangulation2_dual_edges<Regular2>,"dual edges coordinates")
	;
	class_<Delaunay3>( "Delaunay3" )
	.constructor()
	.method("insert",&Delaunay3_insert,"insert points")
	.method("remove_at_pos",&Triangulation_remove_at_pos<Delaunay3>,"remove point at position")
	.method("remove_neighbour_of",&Delaunay3_remove_neighbour_of,"remove point neighbour of point")
	.method("vertices",&Triangulation3_vertices<Delaunay3>,"vertices coordinates")
	.method("edges",&Triangulation3_edges<Delaunay3>,"edges coordinates")
	.method("facets",&Triangulation3_facets<Delaunay3>,"facets coordinates")
	.method("cells",&Triangulation3_cells<Delaunay3>,"cells coordinates")
	.method("dual_vertices",&Triangulation3_dual_vertices<Delaunay3>,"dual vertices coordinates")
	.method("dual_edges",&Triangulation3_dual_edges<Delaunay3>,"dual edges coordinates")
	.method("conflicted_cells",&Delaunay3_conflicted_cells,"conflicted cells")
	.method("conflicted_cells_and_boundary_facets",&Delaunay3_conflicted_cells_and_boundary_facets,"conflicted cells and boundary facets")
	.method("conflicted_edges_and_boundary_edges",&Delaunay3_conflicted_edges_and_boundary_edges,"conflicted and boundary edges")
	//.method("show_vertices",&Delaunay_show_vertices,"vertices coordinates")
	;
	class_<Regular3>( "Regular3" )
	.constructor()
	.method("insert",&Regular3_insert,"insert points")
	.method("remove_at_pos",&Triangulation_remove_at_pos<Regular3>,"remove point at position")
	//.method("remove_neighbour_of",&Delaunay3_remove_neighbour_of,"remove point neighbour of point")
	.method("vertices",&Triangulation3_vertices<Regular3>,"vertices coordinates")
	.method("edges",&Triangulation3_edges<Regular3>,"edges coordinates")
	.method("facets",&Triangulation3_facets<Regular3>,"facets coordinates")
	.method("dual_edges",&Triangulation3_dual_edges<Regular3>,"dual edges coordinates")
	//.method("show_vertices",&Delaunay_show_vertices,"vertices coordinates")
	;
}