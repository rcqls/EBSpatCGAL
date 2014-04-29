#include "cgal_spatstat_triangulation.h"



std::vector<Delaunay2::Vertex_handle> CGAL_Delaunay2_incident_vertices(Delaunay2* obj, Delaunay2::Vertex_handle v) {
	std::vector<Delaunay2::Vertex_handle> incidentVertices;

	Delaunay2::Vertex_circulator vc=obj->incident_vertices(v),done(vc);
	incidentVertices.reserve(circulator_size(vc));
	//DEBUG: std::cout << "incident0" << std::endl;
	if (vc != 0) {	 
		do {
			//DEBUG: std::cout << "incident" << std::endl;
			if(!obj->is_infinite(vc)) {
				incidentVertices.push_back(vc); 
			}  	 
		} while(++vc != done);
	}
	return incidentVertices;
}

std::vector<Delaunay2::Vertex_handle> CGAL_Delaunay2_conflicted_vertices(Delaunay2* obj, Point_2 p) {

	std::vector<Delaunay2::Edge> edges;
 	obj->get_boundary_of_conflicts(p,std::back_inserter(edges));

 	int i=0; 

	std::vector<Delaunay2::Vertex_handle> conflictedVertices;
	conflictedVertices.reserve(edges.size()); 

	for(std::vector<Delaunay2::Edge>::iterator eit = edges.begin();
       eit != edges.end();
       ++eit,++i){
		Delaunay2::Vertex_handle v0=((*eit).first->vertex(((*eit).second+1)%3));
		if(!obj->is_infinite(v0)) {
			conflictedVertices.push_back(v0); 
		}
	}
	return conflictedVertices;
}

//2D Delaunay edges components: all needed for edges when inserting a point

std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> CGAL_Delaunay2_conflicted_and_boundary_edges(Delaunay2* obj, Point_2 p) {

	std::vector<Delaunay2::Face_handle> faces;
	std::vector<Delaunay2::Edge> edges;
 	obj->get_conflicts_and_boundary (p, std::back_inserter(faces),std::back_inserter(edges));

 	int i=0; 

	std::set<Delaunay2_VertexSet> boundaryEdges;

	for(std::vector<Delaunay2::Edge>::iterator eit = edges.begin();
       eit != edges.end();
       ++eit,++i){
		//Delaunay2::Vertex_handle v0=((*eit).first->vertex((*eit).first->cw((*eit).second))),v1=((*eit).first->vertex((*eit).first->ccw((*eit).second)));
		Delaunay2::Vertex_handle v0=((*eit).first->vertex(((*eit).second+1)%3)),v1=((*eit).first->vertex(((*eit).second+2)%3));
		if(!obj->is_infinite(v0) && !obj->is_infinite(v1)) {
			Delaunay2_VertexSet e0;
			e0.insert(v0);e0.insert(v1);boundaryEdges.insert(e0);
		}
	}

	//DEBUG: std::cout << "Number of boundary edges=" << i << std::endl;

	std::set<Delaunay2_VertexSet> conflictedEdges;

	i=0;
	for(std::vector<Delaunay2::Face_handle>::iterator fit = faces.begin();
       fit != faces.end();
       ++fit,++i){
		Delaunay2::Vertex_handle v0=static_cast<Delaunay2::Face_handle>(*fit)->vertex(0),v1=static_cast<Delaunay2::Face_handle>(*fit)->vertex(1),v2=static_cast<Delaunay2::Face_handle>(*fit)->vertex(2);
		if(!obj->is_infinite(v0) && !obj->is_infinite(v1)) {
			Delaunay2_VertexSet e0;
			e0.insert(v0);e0.insert(v1);
			if(boundaryEdges.find(e0) == boundaryEdges.end()) conflictedEdges.insert(e0);
		}
		if(!obj->is_infinite(v0) && !obj->is_infinite(v2)) {
			Delaunay2_VertexSet e1;
			e1.insert(v0);e1.insert(v2);
			if(boundaryEdges.find(e1) == boundaryEdges.end()) conflictedEdges.insert(e1);
		}
		if(!obj->is_infinite(v1) && !obj->is_infinite(v2)) {
			Delaunay2_VertexSet e2;
			e2.insert(v1);e2.insert(v2);
			if(boundaryEdges.find(e2) == boundaryEdges.end()) conflictedEdges.insert(e2);
		}
	}

	//DEBUG: std::cout << "Number of conflicted edges=" << 3*i << " (amoung " << faces.size() << " faces)" << std::endl;

	std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> ret;
	ret = std::make_pair(conflictedEdges,boundaryEdges);
	return ret;
}  

Delaunay2_VertexSet_Set CGAL_Delaunay2_incident_edges(Delaunay2* obj, Delaunay2::Vertex_handle v) {
	std::vector<Delaunay2::Vertex_handle> V;
	Delaunay2_VertexSet_Set incidentEdges;

	Delaunay2::Vertex_circulator vc=obj->incident_vertices(v),done(vc);
	//DEBUG: std::cout << "incident0" << std::endl;
	if (vc != 0) {	 
		do {
			//DEBUG: std::cout << "incident" << std::endl;
			if(!obj->is_infinite(vc)) {
				Delaunay2_VertexSet e;
				e.insert(v);e.insert(vc);
				incidentEdges.insert(e);
			}  	 
		} while(++vc != done);
	}
	return incidentEdges;
}

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
                     std::back_inserter(V)	// Conflict cells in V
    );

	int i=0;  

	Delaunay3_VertexSet_Set boundaryEdges;

	for(
		std::vector<Delaunay3::Facet>::iterator fit = f.begin();
       	fit != f.end();
       	++fit,++i)
	{
		Delaunay3::Vertex_handle v0=((*fit).first->vertex(((*fit).second+1)%4)),v1=((*fit).first->vertex(((*fit).second+2)%4)),v2=((*fit).first->vertex(((*fit).second+3)%4));
		
		Delaunay3_VertexSet e0;
		e0.insert(v0);e0.insert(v1);boundaryEdges.insert(e0);
		Delaunay3_VertexSet e1;
		e1.insert(v0);e1.insert(v2);boundaryEdges.insert(e1);
		Delaunay3_VertexSet e2;
		e2.insert(v1);e2.insert(v2);boundaryEdges.insert(e2);
	}

	//DEBUG: std::cout << "Number of boundary edges=" << 3*i << " (amoung " << f.size() << " facets)" << std::endl;

	Delaunay3_VertexSet_Set conflictedEdges;

	i=0;
	for(
		std::vector<Delaunay3::Cell_handle>::iterator cit = V.begin();
       	cit != V.end();
       	++cit,++i)
	{
		Delaunay3::Vertex_handle v0=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(0),v1=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(1),v2=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(2),v3=static_cast<Delaunay3::Cell_handle>(*cit)->vertex(3);
		if(!obj->is_infinite(v0) && !obj->is_infinite(v1)) {
			Delaunay3_VertexSet e0;
			e0.insert(v0);e0.insert(v1);
			if(boundaryEdges.find(e0) == boundaryEdges.end()) conflictedEdges.insert(e0);
		}
		if(!obj->is_infinite(v0) && !obj->is_infinite(v2)) {
			Delaunay3_VertexSet e1;
			e1.insert(v0);e1.insert(v2);
			if(boundaryEdges.find(e1) == boundaryEdges.end()) conflictedEdges.insert(e1);
		}
		if(!obj->is_infinite(v0) && !obj->is_infinite(v3)) {
			Delaunay3_VertexSet e2;
			e2.insert(v0);e2.insert(v3);
			if(boundaryEdges.find(e2) == boundaryEdges.end()) conflictedEdges.insert(e2);
		}
		if(!obj->is_infinite(v1) && !obj->is_infinite(v2)) {
			Delaunay3_VertexSet e3;
			e3.insert(v1);e3.insert(v2);
			if(boundaryEdges.find(e3) == boundaryEdges.end()) conflictedEdges.insert(e3);
		}
		if(!obj->is_infinite(v1) && !obj->is_infinite(v3)) {
			Delaunay3_VertexSet e4;
			e4.insert(v1);e4.insert(v3);
			if(boundaryEdges.find(e4) == boundaryEdges.end()) conflictedEdges.insert(e4);
		}
		if(!obj->is_infinite(v2) && !obj->is_infinite(v3)) {
			Delaunay3_VertexSet e5;
			e5.insert(v2);e5.insert(v3);
			if(boundaryEdges.find(e5) == boundaryEdges.end()) conflictedEdges.insert(e5);
		}
	}

	//DEBUG: std::cout << "Number of conflicted edges=" << 6*i << " (amoung " << V.size() << " cells)" << std::endl;

	std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> ret;
	ret = std::make_pair(conflictedEdges,boundaryEdges);
	return ret;
}  

Delaunay3_VertexSet_Set CGAL_Delaunay3_incident_edges(Delaunay3* obj, Delaunay3::Vertex_handle v) {
	std::vector<Delaunay3::Vertex_handle> V;
	Delaunay3_VertexSet_Set incidentEdges;

	obj->adjacent_vertices(v,std::back_inserter(V));
	for(
		std::vector<Delaunay3::Vertex_handle>::iterator vit=V.begin();
		vit != V.end();
		++vit) 
	{
		if(!obj->is_infinite(*vit)) {
			Delaunay3_VertexSet e;
			e.insert(v);e.insert(*vit);
			incidentEdges.insert(e);
		}
	}

	return incidentEdges;
}