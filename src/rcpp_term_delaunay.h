#include "rcpp_spatstat_triangulation.h"
#include "rcpp_term_expr.h"

typedef TermType<DEL2,Delaunay2,Point_2,Del2D_Vertex_handle,Delaunay2_VertexSet_Set,2> Del2TermType2D;
typedef TermType<DEL2,Delaunay3,Point_3,Del3D_Vertex_handle,Delaunay3_VertexSet_Set,3> Del2TermType3D;

RCPP_EXPOSED_AS(Del2TermType2D)
RCPP_EXPOSED_WRAP(Del2TermType2D)
RCPP_EXPOSED_AS(Del2TermType3D)
RCPP_EXPOSED_WRAP(Del2TermType3D)

//specialisation methods for Del2TermType2D
template<>
List Del2TermType2D::update_infos(Delaunay2_VertexSet_Set set) {
	int i=0;
	List ret(set.size());
	for(
		Delaunay2_VertexSet_Set::iterator eit = set.begin();
		eit != set.end();
		++eit,++i
	) {
		Del2D_Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
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
				if(structure->is_infinite(v0)) {
			      x0[0]=NA_REAL;
			      x0[1]=NA_REAL;
			    } else {
			      x0[0]=p0.x();
			      x0[1]=p0.y();
			    }
			    if(structure->is_infinite(v1)) {
			      x1[0]=NA_REAL;
			      x1[1]=NA_REAL;
			    } else {
			      x1[0]=p1.x();
			      x1[1]=p1.y();
			    }
			    res["x"]=List::create(x0,x1);
			} else if (info=="l2") {
				if(structure->is_infinite(v0) || structure->is_infinite(v1))
					res["l2"]=NA_REAL; 
				else
					res["l2"]=pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2);

			} else if (info=="l") {
				if(structure->is_infinite(v0) || structure->is_infinite(v1))
					res["l"]=NA_REAL; 
				else
					res["l"]=sqrt(pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2));

			} 
			ret[i]=res;
		}

	}
	return ret;
}

template<>
List Del2TermType2D::update_infos(std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> sets) {
	return Del2TermType2D::update_infos(sets.first);
}

template <>
List Del2TermType2D::make_before_list() {
	std::pair<Delaunay2_VertexSet_Set,Delaunay2_VertexSet_Set> conflictedEdges;
  	conflictedEdges=CGAL_Delaunay2_conflicted_and_boundary_edges(structure,current);
  	//prepare the negative list
  	return update_infos(conflictedEdges);
}	

template <>
List Del2TermType2D::make_after_list() {
	Delaunay2_VertexSet_Set incidentEdges;
  	incidentEdges=CGAL_Delaunay2_incident_edges(structure,current_handle);
  	//prepare the positive list
  	return update_infos(incidentEdges);
}

template <> template <>
void Del2TermType2D::make_local_lists<INSERTION>() {
	//before INSERTION
	locBefore=Del2TermType2D::make_before_list();

  	//INSERTION
  	current_handle=structure->insert(current);
  	
  	//after INSERTION
  	locAfter=Del2TermType2D::make_after_list();
  	//as before
  	structure->remove(current_handle);
}

template <> template <>
void Del2TermType2D::make_local_lists<DELETION>() {
	//before DELETION
  	 
  	locAfter= Del2TermType2D::make_after_list();
  	//DELETION
  	structure->remove(current_handle);
	//after DELETION
	 
  	locBefore= Del2TermType2D::make_before_list();
  	//As before!
  	structure->insert(current);
  	
}

template <> template<>
void Del2TermType2D::set_current_<INSERTION>(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_2(p[0],p[1]);
	//create the lists of edges
	make_local_lists<INSERTION>();
}

template <> template<>
void Del2TermType2D::set_current_<DELETION>(NumericVector p) {
	double i=p[0];
	current_index=i-1;
	if(current_index <0 || current_index >= structure->number_of_vertices()) return;
	std::cout << "current_index=" << current_index <<std::endl;
	current_handle=Triangulation_vertex_at_pos<Delaunay2>(structure,current_index);
	
	current=current_handle->point();
	//create the lists of edges
	make_local_lists<DELETION>();
}

template <>
void Del2TermType2D::set_current(NumericVector p) {
	mode= p.size()==1 ? DELETION : INSERTION;
    switch(mode) {
    case INSERTION: 
        set_current_<INSERTION>(p);
        break;
    case DELETION: 
        set_current_<DELETION>(p);
    }
}



template <> 
NumericVector Del2TermType2D::get_current() {
	return NumericVector::create(current.x(),current.y());
}


//specialisation methods for Del2TermType3D

template<>
List Del2TermType3D::update_infos(Delaunay3_VertexSet_Set set) {
	int i=0;
	List ret(set.size());
	for(
		Delaunay3_VertexSet_Set::iterator eit = set.begin();
		eit != set.end();
		++eit,++i
	) {
		Del3D_Vertex_handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
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
				// if(structure->is_infinite(v0)) {
			 //      x0[0]=NA_REAL;
			 //      x0[1]=NA_REAL;
			 //      x0[2]=NA_REAL;
			 //    } else {
			      x0[0]=p0.x();
			      x0[1]=p0.y();
			      x0[2]=p0.z();
			    // }
			    // if(structure->is_infinite(v1)) {
			    //   x1[0]=NA_REAL;
			    //   x1[1]=NA_REAL;
			    //   x1[2]=NA_REAL;
			    // } else {
			      x1[0]=p1.x();
			      x1[1]=p1.y();
			      x1[2]=p1.z();
			    // }
			    res["x"]=List::create(x0,x1);
			} else if (info=="l2") {
				//if(structure->is_infinite(v0) || structure->is_infinite(v1))
				//	res["l2"]=NA_REAL; 
				//else
					res["l2"]=pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2) + pow(p0.z()-p1.z(),2);

			} else if (info=="l") {
				// if(structure->is_infinite(v0) || structure->is_infinite(v1))
				// 	res["l"]=NA_REAL; 
				// else
					res["l"]=sqrt(pow(p0.x()-p1.x(),2) + pow(p0.y()-p1.y(),2) + pow(p0.z()-p1.z(),2));

			} 
			ret[i]=res;
		}

	}
	return ret;
}

template<>
List Del2TermType3D::update_infos(std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> sets) {
	return Del2TermType3D::update_infos(sets.first);
}


template <>
List Del2TermType3D::make_before_list() {
	std::pair<Delaunay3_VertexSet_Set,Delaunay3_VertexSet_Set> conflictedEdges;
  	conflictedEdges=CGAL_Delaunay3_conflicted_and_boundary_edges(structure,current);
  	//prepare the negative list
  	return update_infos(conflictedEdges);
}	

template <>
List Del2TermType3D::make_after_list() {
	Delaunay3_VertexSet_Set incidentEdges;
  	incidentEdges=CGAL_Delaunay3_incident_edges(structure,current_handle);
  	//prepare the positive list
  	return update_infos(incidentEdges);
}

template <> template <>
void Del2TermType3D::make_local_lists<INSERTION>() {
	//before INSERTION
	locBefore=Del2TermType3D::make_before_list();

  	//INSERTION
  	apply_insert();
  	
  	//after INSERTION
  	locAfter=Del2TermType3D::make_after_list();
  	//as before
  	apply_delete();
}

template <> template <>
void Del2TermType3D::make_local_lists<DELETION>() {
	//before DELETION
  	 
  	locAfter= Del2TermType3D::make_after_list();
  	//DELETION
  	apply_delete();
	//after DELETION
	 
  	locBefore= Del2TermType3D::make_before_list();
  	//As before!
  	apply_insert();
  	
}

template <> template<>
void Del2TermType3D::set_current_<INSERTION>(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_3(p[0],p[1],p[2]);
	//create the lists of edges
	make_local_lists<INSERTION>();
}

template <> template<>
void Del2TermType3D::set_current_<DELETION>(NumericVector p) {
	double i=p[0];
	current_index=i-1;
	if(current_index <0 || current_index >= structure->number_of_vertices()) return;
	std::cout << "current_index=" << current_index <<std::endl;
	current_handle=Triangulation_vertex_at_pos<Delaunay3>(structure,current_index);
	current=current_handle->point();
	//create the lists of edges
	make_local_lists<DELETION>();
}

template <>
void Del2TermType3D::set_current(NumericVector p) {
	mode= p.size()==1 ? DELETION : INSERTION;
    switch(mode) {
    case INSERTION: 
        set_current_<INSERTION>(p);
        break;
    case DELETION: 
        set_current_<DELETION>(p);
    }
}

template <> 
NumericVector Del2TermType3D::get_current() {
	return NumericVector::create(current.x(),current.y(),current.z());
}