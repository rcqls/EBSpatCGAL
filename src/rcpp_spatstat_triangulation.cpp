#include <Rcpp.h>
#include "rcpp_spatstat_triangulation.h"

using namespace Rcpp;

// helpers
void Delaunay2_insert_one_with_info( Delaunay2* obj, NumericVector xy, List info ) {
  Point_2 point(xy[0],xy[1]);
  Delaunay2::Vertex_handle vh=obj->insert(point);
  //R_PreserveObject(info);
  vh->info() = info;
}

template <typename TRIANGULATION>
void Triangulation_update_infinte_vertex_info(TRIANGULATION* obj, List info ) {
  typename TRIANGULATION::Vertex_handle vh=obj->infinite_vertex();
  //R_PreserveObject(info);
  vh->info() = info;
}

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

void Delaunay3_insert_one_with_info( Delaunay3* obj, NumericVector xyz, List info ) {
  Point_3 point(xyz[0],xyz[1],xyz[2]);
  Delaunay3::Vertex_handle vh=obj->insert(point);
  vh->info() = info;
}

IntegerVector  Delaunay3_insert( Delaunay3* obj, NumericVector ptsX, NumericVector ptsY, NumericVector ptsZ ) {
	std::vector<Point_3> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
    	points.push_back(Point_3(ptsX[i],ptsY[i],ptsZ[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
	return IntegerVector::create(obj->number_of_vertices());
}

IntegerVector Regular2_insert( Regular2* obj, NumericVector ptsX, NumericVector ptsY,  NumericVector ptsW) {
	std::vector<Point_2> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
    	points.push_back(Weighted_point_2(Point_2(ptsX[i],ptsY[i]),ptsW[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
	return IntegerVector::create(obj->number_of_vertices());
}

IntegerVector Regular3_insert( Regular3* obj, NumericVector ptsX, NumericVector ptsY, NumericVector ptsZ,  NumericVector ptsW) {
	std::vector<Weighted_point_3> points;
	int nbPts=ptsX.size();

	std::cout << "Number of vertices to insert is " << nbPts << std::endl;
  	points.reserve(nbPts);
  	for(int i = 0; i < nbPts; ++i){
		points.push_back(Weighted_point_3(Point_3(ptsX[i],ptsY[i],ptsZ[i]),ptsW[i]));
  	}
	obj->insert( points.begin(), points.end() );
	std::cout << "Number of inserted vertices is " << obj->number_of_vertices() << std::endl;
	return IntegerVector::create(obj->number_of_vertices());
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

// TODO: extension
template <typename TRIANGULATION2>
void Triangulation2_remove_inside( TRIANGULATION2* obj, NumericVector xy) {

  Point_2 p(xy[0],xy[1]),q(xy[2],xy[3]); //to extend to polygon

  Rect_2 rect(p,q);

  for(typename TRIANGULATION2::Finite_vertices_iterator 
          vit = obj->finite_vertices_begin(),
          end = obj->finite_vertices_end();
        vit!= end; ++vit)
    {
       if(rect.has_on_bounded_side(vit->point())) obj->remove(vit);
    }

}

template <typename TRIANGULATION3>
void Triangulation3_remove_inside( TRIANGULATION3* obj, NumericVector xy) {

  Point_3 p(xy[0],xy[1],xy[2]),q(xy[3],xy[4],xy[5]); //to extend to polygon

  Cuboid_3 cuboid(p,q);

  for(typename TRIANGULATION3::Finite_vertices_iterator 
          vit = obj->finite_vertices_begin(),
          end = obj->finite_vertices_end();
        vit!= end; ++vit)
    {
       if(cuboid.has_on_bounded_side(vit->point())) obj->remove(vit);
    }

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

template <typename TRIANGULATION>
List Triangulation_vertices_infos(TRIANGULATION* obj) {

  List infos(obj->number_of_vertices());
  int i=0;
  for(typename TRIANGULATION::Finite_vertices_iterator 
          vit = obj->finite_vertices_begin(),
          end = obj->finite_vertices_end();
        vit!= end; ++vit,++i)
    {
       infos[i]=vit->info();
    }
    return infos;
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

//OBSOLETE SOON -> replaced by Delaunay2_conflicted_and_boundary_edges
List Delaunay2_conflicted_edges_and_boundary_edges(Delaunay2* obj, NumericVector xy) {
  std::list<Delaunay2::Face_handle> faces;
  std::list<Delaunay2::Edge> edges;
  typedef std::set<Delaunay2::Vertex_handle> Vertex_2_Set;
  Point_2 p(xy[0],xy[1]);
  obj->get_conflicts_and_boundary (p, std::back_inserter(faces),std::back_inserter(edges));

  int i=0; 

  std::set<Vertex_2_Set> boundaryEdges;

  for(std::list<Delaunay2::Edge>::iterator eit = edges.begin();
    eit != edges.end();
    ++eit,++i)
  {
    //Delaunay2::Vertex_handle v0=((*eit).first->vertex((*eit).first->cw((*eit).second))),v1=((*eit).first->vertex((*eit).first->ccw((*eit).second)));
    Delaunay2::Vertex_handle v0=((*eit).first->vertex(((*eit).second+1)%3)),v1=((*eit).first->vertex(((*eit).second+2)%3));
   
    Vertex_2_Set e0;
    e0.insert(v0);e0.insert(v1);boundaryEdges.insert(e0);
  }

  std::cout << "Number of boundary edges=" << i << std::endl;

  std::set<Vertex_2_Set> conflictedEdges;

  i=0;
  for(std::list<Delaunay2::Face_handle>::iterator fit = faces.begin();
        fit != faces.end();
        ++fit,++i)
  {
    Delaunay2::Vertex_handle v0=static_cast<Delaunay2::Face_handle>(*fit)->vertex(0),v1=static_cast<Delaunay2::Face_handle>(*fit)->vertex(1),v2=static_cast<Delaunay2::Face_handle>(*fit)->vertex(2);
    Vertex_2_Set e0;
    e0.insert(v0);e0.insert(v1);
    if(boundaryEdges.find(e0) == boundaryEdges.end()) conflictedEdges.insert(e0);
    Vertex_2_Set e1;
    e1.insert(v0);e1.insert(v2);
    if(boundaryEdges.find(e1) == boundaryEdges.end()) conflictedEdges.insert(e1);
    Vertex_2_Set e2;
    e2.insert(v1);e2.insert(v2);
    if(boundaryEdges.find(e2) == boundaryEdges.end()) conflictedEdges.insert(e2);
  }

  std::cout << "Number of conflicted edges=" << 3*i << std::endl;

	// This is the R export!
	NumericMatrix be(boundaryEdges.size(),4);
	NumericMatrix ce(conflictedEdges.size(),4);

	i=0;
	for(std::set<Vertex_2_Set>::iterator eit = boundaryEdges.begin();
       eit != boundaryEdges.end();
       ++eit,++i) {
       	Delaunay2::Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
       	Point_2 p0=v0->point(),p1=v1->point();
       	if(obj->is_infinite(v0)) {
			be(i,0)=NA_REAL;
     		be(i,1)=NA_REAL;
		} else {
    		be(i,0)=p0.x();
     		be(i,1)=p0.y();
     	}
     	if(obj->is_infinite(v1)) {
			be(i,2)=NA_REAL;
     		be(i,3)=NA_REAL;
		} else {
    		be(i,2)=p1.x();
     		be(i,3)=p1.y();
     	}
		 
 	}

 	i=0;
 	for(std::set<Vertex_2_Set>::iterator eit = conflictedEdges.begin();
       eit != conflictedEdges.end();
       ++eit,++i){
		Delaunay2::Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
       	Point_2 p0=v0->point(),p1=v1->point();
       	if(obj->is_infinite(v0)) {
			ce(i,0)=NA_REAL;
     		ce(i,1)=NA_REAL;
		} else {
    		ce(i,0)=p0.x();
     		ce(i,1)=p0.y();
     	}
     	if(obj->is_infinite(v1)) {
			ce(i,2)=NA_REAL;
     		ce(i,3)=NA_REAL;
		} else {
    		ce(i,2)=p1.x();
     		ce(i,3)=p1.y();
     	}
 	}

	List ret; ret["conflicted_edges"] = ce; ret["boundary_edges"] = be;
	return ret; 

 
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

//OBSOLETE SOON
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

//same as above with circles (center + radius)
NumericMatrix Delaunay2_conflicted_faces_with_circles(Delaunay2* obj, NumericVector xy) {
	std::list<Delaunay2::Face_handle> faces;
 	Point_2 p(xy[0],xy[1]);
 	obj->find_conflicts (p, faces);

 	int i=0,nb=iterator_distance(faces.begin(),faces.end()); 

 	NumericMatrix fc(nb,9);

	for(
		std::list<Delaunay2::Face_handle>::iterator fit = faces.begin();
     	fit != faces.end();
      	++fit,++i
    ) {
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
//**************************************************************************************//
//New try!!! Above is old code, below is new code (using cgal_spatstat_triangulation.h) //
//**************************************************************************************//
//2D Delaunay
NumericMatrix Delaunay2_VertexSet_Set_To_R(Delaunay2_VertexSet_Set eSet,Delaunay2* del2) {

  NumericMatrix es(eSet.size(),4);

  int i=0;
  for(
    Delaunay2_VertexSet_Set::iterator eit = eSet.begin();
    eit != eSet.end();
    ++eit,++i
  ) {
    Delaunay2::Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
    Point_2 p0=v0->point(),p1=v1->point();
    if(del2->is_infinite(v0)) {
      es(i,0)=NA_REAL;
      es(i,1)=NA_REAL;
    } else {
      es(i,0)=p0.x();
      es(i,1)=p0.y();
    }
    if(del2->is_infinite(v1)) {
      es(i,2)=NA_REAL;
      es(i,3)=NA_REAL;
    } else {
      es(i,2)=p1.x();
      es(i,3)=p1.y();
    }
     
  }
  return es;
}

List Delaunay2_conflicted_and_boundary_edges(Delaunay2* obj, NumericVector xy) {
  Point_2 p(xy[0],xy[1]);

  std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> vertexSets;
  vertexSets=CGAL_Delaunay2_conflicted_and_boundary_edges(obj,p);

  //std::cout << "ICI" << std::endl;

  Delaunay2_VertexSet_Set boundaryEdges=vertexSets.second,conflictedEdges=vertexSets.first;
  // This is the R export!
  NumericMatrix be(boundaryEdges.size(),4);
  NumericMatrix ce(conflictedEdges.size(),4);

  List ret; 
  ret["conflicted_edges"] = Delaunay2_VertexSet_Set_To_R(conflictedEdges,obj); 
  ret["boundary_edges"] = Delaunay2_VertexSet_Set_To_R(boundaryEdges,obj);
  return ret;
}

NumericMatrix Delaunay2_incident_edges(Delaunay2* obj, IntegerVector rank) {
  int n=rank[0]-1;

  if(n >=0 && n < obj->number_of_vertices()) {
    Delaunay2::Finite_vertices_iterator vit=obj->finite_vertices_begin();
    for(int i=0;i<n;++i) ++vit;

    Delaunay2_VertexSet_Set incidentEdges=CGAL_Delaunay2_incident_edges(obj,vit);
    NumericMatrix ie=Delaunay2_VertexSet_Set_To_R(incidentEdges,obj); 
    return ie;
  }
  NumericMatrix ie(0,0);
  return ie;
}

//3D Delaunay

NumericMatrix Delaunay3_VertexSet_Set_To_R(Delaunay3_VertexSet_Set eSet,Delaunay3* del3) {

  NumericMatrix es(eSet.size(),6);

  int i=0;
  for(
    Delaunay3_VertexSet_Set::iterator eit = eSet.begin();
    eit != eSet.end();
    ++eit,++i
  ) {
    Delaunay3::Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
    Point_3 p0=v0->point(),p1=v1->point();
    if(del3->is_infinite(v0)) {
      es(i,0)=NA_REAL;
      es(i,1)=NA_REAL;
      es(i,2)=NA_REAL;
    } else {
      es(i,0)=p0.x();
      es(i,1)=p0.y();
      es(i,2)=p0.z();
    }
    if(del3->is_infinite(v1)) {
      es(i,3)=NA_REAL;
      es(i,4)=NA_REAL;
      es(i,5)=NA_REAL;
    } else {
      es(i,3)=p1.x();
      es(i,4)=p1.y();
      es(i,5)=p1.z();
    }
     
  }
  return es;
}



List Delaunay3_conflicted_and_boundary_edges(Delaunay3* obj, NumericVector xy) {
  Point_3 p(xy[0],xy[1],xy[2]);

  std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> vertexSets;
  vertexSets=CGAL_Delaunay3_conflicted_and_boundary_edges(obj,p);

  Delaunay3_VertexSet_Set boundaryEdges=vertexSets.second,conflictedEdges=vertexSets.first;
  // This is the R export!
  NumericMatrix be(boundaryEdges.size(),6);
  NumericMatrix ce(conflictedEdges.size(),6);

  List ret; 
  ret["conflicted_edges"] = Delaunay3_VertexSet_Set_To_R(conflictedEdges,obj); 
  ret["boundary_edges"] = Delaunay3_VertexSet_Set_To_R(boundaryEdges,obj);
  return ret;
}

NumericMatrix Delaunay3_incident_edges(Delaunay3* obj, IntegerVector rank) {
	int n=rank[0]-1;

	if(n >=0 && n < obj->number_of_vertices()) {
		Delaunay3::Finite_vertices_iterator vit=obj->finite_vertices_begin();
		for(int i=0;i<n;++i) ++vit;

    Delaunay3_VertexSet_Set incidentEdges=CGAL_Delaunay3_incident_edges(obj,vit);
		NumericMatrix ie=Delaunay3_VertexSet_Set_To_R(incidentEdges,obj); 
  	return ie;
	}
	NumericMatrix ie(0,0);
	return ie;
}


RCPP_MODULE(cgal_module) {
	class_<Delaunay2>( "Delaunay2" )
	.constructor()
  .method("insert_one_with_info",&Delaunay2_insert_one_with_info,"insert points")
	.method("insert",&Delaunay2_insert,"insert points")
  .method("update_infinite_vertex_info",&Triangulation_update_infinte_vertex_info<Delaunay2>,"update infinite vertex info")
	.method("remove_at_pos",&Triangulation_remove_at_pos<Delaunay2>,"remove point at position")
  .method("remove_inside",&Triangulation2_remove_inside<Delaunay2>,"remove inside rect")
	.method("remove_neighbour_of",&Delaunay2_remove_neighbour_of,"remove point neighbour of point")
	.method("vertices",&Triangulation2_vertices<Delaunay2>,"vertices coordinates")
  .method("vertices_infos",&Triangulation_vertices_infos<Delaunay2>,"vertices infos")
	.method("edges",&Triangulation2_edges<Delaunay2>,"edges coordinates")
	.method("dual_vertices",&Triangulation2_dual_vertices<Delaunay2>,"dual vertices coordinates")
	.method("dual_edges",&Triangulation2_dual_edges<Delaunay2>,"dual edges coordinates")
	.method("conflicted_faces",&Delaunay2_conflicted_faces,"conflicted faces")
	.method("conflicted_faces_with_circles",&Delaunay2_conflicted_faces_with_circles,"conflicted faces with circles")
  /*OBSOLETE SOON*/.method("conflicted_edges_and_boundary_edges",&Delaunay2_conflicted_edges_and_boundary_edges,"conflicted and boundary edges")
	.method("conflicted_and_boundary_edges",&Delaunay2_conflicted_and_boundary_edges,"conflicted and boundary edges")
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
  .method("insert_one_with_info",&Delaunay3_insert_one_with_info,"insert points")
	.method("insert",&Delaunay3_insert,"insert points")
	.method("remove_at_pos",&Triangulation_remove_at_pos<Delaunay3>,"remove point at position")
	.method("remove_inside",&Triangulation3_remove_inside<Delaunay3>,"remove inside rect")
  .method("remove_neighbour_of",&Delaunay3_remove_neighbour_of,"remove point neighbour of point")
	.method("vertices",&Triangulation3_vertices<Delaunay3>,"vertices coordinates")
	.method("vertices_infos",&Triangulation_vertices_infos<Delaunay3>,"vertices infos")
  .method("edges",&Triangulation3_edges<Delaunay3>,"edges coordinates")
	.method("facets",&Triangulation3_facets<Delaunay3>,"facets coordinates")
	.method("cells",&Triangulation3_cells<Delaunay3>,"cells coordinates")
	.method("dual_vertices",&Triangulation3_dual_vertices<Delaunay3>,"dual vertices coordinates")
	.method("dual_edges",&Triangulation3_dual_edges<Delaunay3>,"dual edges coordinates")
	.method("conflicted_cells",&Delaunay3_conflicted_cells,"conflicted cells")
  /*OBSOLETE SOON*/.method("conflicted_cells_and_boundary_facets",&Delaunay3_conflicted_cells_and_boundary_facets,"conflicted cells and boundary facets")
	.method("conflicted_and_boundary_edges",&Delaunay3_conflicted_and_boundary_edges,"conflicted and boundary edges")
	.method("incident_edges",&Delaunay3_incident_edges,"incident edges")
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