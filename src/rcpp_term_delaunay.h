#include "rcpp_term_expr.h"

typedef std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> Delaunay2_VertexSet_Sets;
typedef std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> Delaunay3_VertexSet_Sets;

typedef TermType<DEL2,Delaunay2,Point_2,Delaunay2_VertexSet_Sets,2> Del2TermType2D;
typedef TermType<DEL2,Delaunay3,Point_3,Delaunay3_VertexSet_Sets,3> Del2TermType3D;

//specialisation methods for Del2TermType2D
template<>
List Del2TermType2D::update_infos(Delaunay2_VertexSet_Sets eSets) {
	int i=0;
	List ret(eSets.first.size());
	for(
		Delaunay2_VertexSet_Set::iterator eit = eSets.first.begin();
		eit != eSets.first.end();
		++eit,++i
	) {
		Delaunay2::Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
		Point_2 p0=v0->point(),p1=v1->point();
		List res;
		for(
			std::vector< std::string >::iterator cit = infos.begin();
			cit != infos.end();
			++cit
		) {
			std::string info=*cit;
			if(info=="x") {
				NumericVector x0(2);
				NumericVector x1(2);
				if(structure.is_infinite(v0)) {
			      x0[0]=NA_REAL;
			      x0[1]=NA_REAL;
			    } else {
			      x0[0]=p0.x();
			      x0[1]=p0.y();
			    }
			    if(structure.is_infinite(v1)) {
			      x1[0]=NA_REAL;
			      x1[1]=NA_REAL;
			    } else {
			      x1[0]=p1.x();
			      x1[1]=p1.y();
			    }
			    res["x"]=List::create(x0,x1);
			} else if (info=="l2") {
				if(structure.is_infinite(v0) || structure.is_infinite(v1))
					res["l2"]=NA_REAL; 
				else
					res["l2"]=pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2);

			} else if (info=="l") {
				if(structure.is_infinite(v0) || structure.is_infinite(v1))
					res["l"]=NA_REAL; 
				else
					res["l"]=sqrt(pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2));

			} 
			ret[i]=res;
		}

	}
	return ret;
}


template <>
void Del2TermType2D::make_local_lists() {
	//before insertion
	std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> conflictedEdges;
  	conflictedEdges=CGAL_Delaunay2_conflicted_and_boundary_edges(&structure,current);
  	//prepare the locBefore list
  	locBefore=update_infos(conflictedEdges);

  	//after insertion
  	Delaunay2::Vertex_handle h=structure.insert(current);
  	Delaunay2_VertexSet_Set incidentEdges;
  	incidentEdges=CGAL_Delaunay2_incident_edges(&structure,h);
  	//prepare the locAfter list
  	locAfter=update_infos(std::make_pair(incidentEdges,conflictedEdges.second));
  	structure.remove(h);
}


template <> 
void Del2TermType2D::set_current(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_2(p[0],p[1]);
	//create the lists of edges
	make_local_lists();
}

template <> 
void Del2TermType2D::set_current_index(int rank) {
	
}

template <> 
NumericVector Del2TermType2D::get_current() {
	return NumericVector::create(current.x(),current.y());
}


//specialisation methods for Del2TermType3D

template<>
List Del2TermType3D::update_infos(Delaunay3_VertexSet_Sets eSets) {
	int i=0;
	List ret(eSets.first.size());
	for(
		Delaunay3_VertexSet_Set::iterator eit = eSets.first.begin();
		eit != eSets.first.end();
		++eit,++i
	) {
		Delaunay3::Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
		Point_3 p0=v0->point(),p1=v1->point();
		List res;
		for(
			std::vector< std::string >::iterator cit = infos.begin();
			cit != infos.end();
			++cit
		) {
			std::string info=*cit;
			if(info=="x") {
				NumericVector x0(3);
				NumericVector x1(3);
				if(structure.is_infinite(v0)) {
			      x0[0]=NA_REAL;
			      x0[1]=NA_REAL;
			      x0[2]=NA_REAL;
			    } else {
			      x0[0]=p0.x();
			      x0[1]=p0.y();
			      x0[2]=p0.z();
			    }
			    if(structure.is_infinite(v1)) {
			      x1[0]=NA_REAL;
			      x1[1]=NA_REAL;
			      x1[2]=NA_REAL;
			    } else {
			      x1[0]=p1.x();
			      x1[1]=p1.y();
			      x1[2]=p1.z();
			    }
			    res["x"]=List::create(x0,x1);
			} else if (info=="l2") {
				if(structure.is_infinite(v0) || structure.is_infinite(v1))
					res["l2"]=NA_REAL; 
				else
					res["l2"]=pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2) + pow(p0.z()-p1.z(),2);

			} else if (info=="l") {
				if(structure.is_infinite(v0) || structure.is_infinite(v1))
					res["l"]=NA_REAL; 
				else
					res["l"]=sqrt(pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2) + pow(p0.z()-p1.z(),2));

			} 
			ret[i]=res;
		}

	}
	return ret;
}


template <>
void Del2TermType3D::make_local_lists() {
	//before insertion
	std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> conflictedEdges;
  	conflictedEdges=CGAL_Delaunay3_conflicted_and_boundary_edges(&structure,current);
  	//prepare the locBefore list
  	locBefore=update_infos(conflictedEdges);

  	//after insertion
  	Delaunay3::Vertex_handle h=structure.insert(current);
  	Delaunay3_VertexSet_Set incidentEdges;
  	incidentEdges=CGAL_Delaunay3_incident_edges(&structure,h);
  	//prepare the locAfter list
  	locAfter=update_infos(std::make_pair(incidentEdges,conflictedEdges.second));
  	structure.remove(h);
}


template <> 
void Del2TermType3D::set_current(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_3(p[0],p[1],p[2]);
	//create the lists of edges
	make_local_lists();
}

template <> 
NumericVector Del2TermType3D::get_current() {
	return NumericVector::create(current.x(),current.y(),current.z());
}