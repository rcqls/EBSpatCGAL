#ifndef RCPP_TERM_DELAUNAY_H
#define RCPP_TERM_DELAUNAY_H
#include "rcpp_spatstat_triangulation.h"
#include "rcpp_term_expr.h"

typedef TermType<DEL1,Delaunay2> Del1TermType2D;
typedef TermType<DEL1,Delaunay3> Del1TermType3D;
typedef TermType<DEL2,Delaunay2> Del2TermType2D;
typedef TermType<DEL2,Delaunay3> Del2TermType3D;
typedef TermType<DEL3,Delaunay2> Del3TermType2D;
typedef TermType<DEL3,Delaunay3> Del3TermType3D;
typedef TermType<DEL4,Delaunay3> Del4TermType3D;

RCPP_EXPOSED_AS(Del1TermType2D)
RCPP_EXPOSED_WRAP(Del1TermType2D)
RCPP_EXPOSED_AS(Del1TermType3D)
RCPP_EXPOSED_WRAP(Del1TermType3D)
RCPP_EXPOSED_AS(Del2TermType2D)
RCPP_EXPOSED_WRAP(Del2TermType2D)
RCPP_EXPOSED_AS(Del2TermType3D)
RCPP_EXPOSED_WRAP(Del2TermType3D)
RCPP_EXPOSED_AS(Del3TermType2D)
RCPP_EXPOSED_WRAP(Del3TermType2D)
RCPP_EXPOSED_AS(Del3TermType3D)
RCPP_EXPOSED_WRAP(Del3TermType3D)
RCPP_EXPOSED_AS(Del4TermType3D)
RCPP_EXPOSED_WRAP(Del4TermType3D)
;


///////////////////////////////////* 2D *//////////////////////////////////
//specialization methods for Del1TermType2D
template<>
List Del1TermType2D::update_infos(std::vector<Delaunay2::Vertex_handle> set) {
	int i=0;
	List ret(set.size());
	for(
		std::vector<Delaunay2::Vertex_handle>::iterator vit = set.begin();
		vit != set.end();
		++vit,++i
	) {
		Delaunay2::Vertex_handle v0=*vit;
		Point_2 p0=v0->point();
		List res;
		for(
			std::vector< std::string >::iterator cit = infos.begin();
			cit != infos.end();
			++cit
		) {
			std::string info=*cit;
			if(info=="x") {
				NumericVector x0(2);
				if(structure->is_infinite(v0)) {
			      x0[0]=NA_REAL;
			      x0[1]=NA_REAL;
			    } else {
			      x0[0]=p0.x();
			      x0[1]=p0.y();
			    }
			    res["x"]=x0;
			} else if (info=="a") {
				double area=CGAL_Delaunay2_cell_area(structure,v0);
				std::cout << "area=" << area << std::endl;
				res["a"]=(area>0 ? NumericVector::create(area) : NumericVector::create(NA_REAL));
			} else if( info == "v") {
				res["v"]=v0->info();
			}
			ret[i]=res;
		}

	}
	return ret;
}


template <>
List Del1TermType2D::make_before_list() {
	std::vector<Delaunay2::Vertex_handle> conflictedVertices;
  	conflictedVertices=CGAL_Delaunay2_conflicted_vertices(structure,current);
  	//prepare the negative list
  	//DEBUG: std::cout << "conflictedEdges=" << conflictedEdges.first.size() << std::endl;
  	return (locBefore=update_infos(conflictedVertices));
}	

template <>
List Del1TermType2D::make_after_list() {
	std::vector<Delaunay2::Vertex_handle> incidentVertices;
  	incidentVertices=CGAL_Delaunay2_incident_vertices(structure,current_handle);
  	//prepare the positive list
  	//DEBUG: std::cout << "incidentEdges=" << incidentEdges.size() << std::endl;
  	return (locAfter=update_infos(incidentVertices));
}

template <> template <>
void Del1TermType2D::make_local_lists<INSERTION>() {
	//before INSERTION
	Del1TermType2D::make_before_list();

  	//INSERTION
  	apply_INSERTION();
  	
  	//after INSERTION
  	Del1TermType2D::make_after_list();
  	//as before
  	apply_DELETION();
}

template <> template <>
void Del1TermType2D::make_local_lists<DELETION>() {
	//before DELETION
  	 
  	Del1TermType2D::make_after_list();
  	//DELETION
  	apply_DELETION();
	//after DELETION
	 
  	Del1TermType2D::make_before_list();
  	//As before!
  	apply_INSERTION();
  	
}

template <> template<>
void Del1TermType2D::set_current_<INSERTION>(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_2(p[0],p[1]);
	//create the lists of edges
	if(auto_make_list) make_local_lists<INSERTION>();
}

template <> template<>
void Del1TermType2D::set_current_<DELETION>(NumericVector p) {
	double i=p[0];
	current_index=i;
	if(current_index <0 || current_index >= structure->number_of_vertices()) {
		//DEBUG: std::cout << "current_index=" << current_index ;
		//DEBUG: std::cout << " not in [0,"<<structure->number_of_vertices()-1<<"]" << std::endl;
		return;
	}
	//DEBUG: std::cout << "current_index=" << current_index <<std::endl;
	current_handle=Triangulation_vertex_at_pos<Delaunay2>(structure,current_index);
	
	current=current_handle->point();
	current_info=current_handle->info();
	//create the lists of edges
	if(auto_make_list) make_local_lists<DELETION>();
}

template <> 
NumericVector Del1TermType2D::get_current() {
	return NumericVector::create(current.x(),current.y());
}


//specialization methods for Del2TermType2D
template<>
List Del2TermType2D::update_infos(HandleSet_Set set) {
	int i=0;
	List ret(set.size());
	for(
		HandleSet_Set::iterator eit = set.begin();
		eit != set.end();
		++eit,++i
	) {
		Handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
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

			}  else if( info == "v") {
				res["v"]=List::create(v0->info(),v1->info());
			} else if( info == "a") {
				NumericVector a(2);
				double area=CGAL_Delaunay2_cell_area(structure,v0);
				a[0]=(area>0 ?  area : NA_REAL);
				area=CGAL_Delaunay2_cell_area(structure,v1);
				a[1]=(area>0 ? area : NA_REAL);
				res["a"]=a;
			}
			ret[i]=res;
		}

	}
	return ret;
}


template <>
List Del2TermType2D::update_infos(std::pair< HandleSet_Set,HandleSet_Set > sets) {
	return Del2TermType2D::update_infos(sets.first);
}

template <>
List Del2TermType2D::make_before_list() {
	std::pair< HandleSet_Set,HandleSet_Set > conflictedEdges;
  	conflictedEdges=CGAL_Delaunay2_conflicted_and_boundary_edges(structure,current);
  	//prepare the negative list
  	//DEBUG: std::cout << "conflictedEdges=" << conflictedEdges.first.size() << std::endl;
  	return (locBefore=update_infos(conflictedEdges));
}	

template <>
List Del2TermType2D::make_after_list() {
	HandleSet_Set incidentEdges;
  	incidentEdges=CGAL_Delaunay2_incident_edges(structure,current_handle);
  	//prepare the positive list
  	//DEBUG: std::cout << "incidentEdges=" << incidentEdges.size() << std::endl;
  	return (locAfter=update_infos(incidentEdges));
}

template <> template <>
void Del2TermType2D::make_local_lists<INSERTION>() {
	//before INSERTION
	Del2TermType2D::make_before_list();

  	//INSERTION
  	apply_INSERTION();
  	
  	//after INSERTION
  	Del2TermType2D::make_after_list();
  	//as before
  	apply_DELETION();
}

template <> template <>
void Del2TermType2D::make_local_lists<DELETION>() {
	//before DELETION
  	 
  	Del2TermType2D::make_after_list();
  	//DELETION
  	apply_DELETION();
	//after DELETION
	 
  	Del2TermType2D::make_before_list();
  	//As before!
  	apply_INSERTION();
  	
}

template <> template<>
void Del2TermType2D::set_current_<INSERTION>(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_2(p[0],p[1]);
	//create the lists of edges
	if(auto_make_list) make_local_lists<INSERTION>();
}

template <> template<>
void Del2TermType2D::set_current_<DELETION>(NumericVector p) {
	double i=p[0];
	current_index=i;
	if(current_index <0 || current_index >= structure->number_of_vertices()) {
		//DEBUG: std::cout << "current_index=" << current_index ;
		//DEBUG: std::cout << " not in [0,"<<structure->number_of_vertices()-1<<"]" << std::endl;
		return;
	}
	//DEBUG: std::cout << "current_index=" << current_index <<std::endl;
	current_handle=Triangulation_vertex_at_pos<Delaunay2>(structure,current_index);
	
	current=current_handle->point();
	current_info=current_handle->info();
	//create the lists of edges
	if(auto_make_list) make_local_lists<DELETION>();
}

template <> 
NumericVector Del2TermType2D::get_current() {
	return NumericVector::create(current.x(),current.y());
}

//specialization methods for Del3TermType2D
template<>
List Del3TermType2D::update_infos(HandleSet_Set set) {
	int i=0;
	List ret(set.size());
	for(
		HandleSet_Set::iterator eit = set.begin();
		eit != set.end();
		++eit,++i
	) {
		Handle v0=*((*eit).begin()),v1=*(++((*eit).begin())),v2=*((*eit).rbegin());
		Point_2 p0=v0->point(),p1=v1->point(),p2=v2->point();
		List res;
		for(
			std::vector< std::string >::iterator cit = infos.begin();
			cit != infos.end();
			++cit
		) {
			std::string info=*cit;
			if(info=="x") { //"id","x","v","a","ta","tp","c","r2","r","sa","ga"
				NumericVector x0(2),x1(2),x2(2);
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
			    if(structure->is_infinite(v2)) {
			      x2[0]=NA_REAL;
			      x2[1]=NA_REAL;
			    } else {
			      x2[0]=p2.x();
			      x2[1]=p2.y();
			    }
			    res["x"]=List::create(x0,x1,x2);
			}  else if( info == "v") {
				res["v"]=List::create(v0->info(),v1->info(),v2->info());
			} else if (info=="ta") {
				if(structure->is_infinite(v0) || structure->is_infinite(v1) || structure->is_infinite(v2))
					res["ta"]=NA_REAL; 
				else
					res["ta"]=std::abs(CGAL::area(p0,p1,p2));
			} else if (info=="c") {
				NumericVector c(2);
				if(structure->is_infinite(v0) || structure->is_infinite(v1) || structure->is_infinite(v2))
					{c[0]=NA_REAL;c[1]=NA_REAL;} 
				else {
					Point_2 pc=CGAL::circumcenter(p0,p1,p2);
					c[0]=pc.x();c[1]=pc.y();
				}
				res["c"]=c;
			} else if (info=="r2") {
				if(structure->is_infinite(v0) || structure->is_infinite(v1) || structure->is_infinite(v2))
					res["r2"]=NA_REAL; 
				else
					res["r2"]=CGAL::squared_radius(p0,p1,p2);

			} else if (info=="r") {
				if(structure->is_infinite(v0) || structure->is_infinite(v1) || structure->is_infinite(v2))
					res["r"]=NA_REAL; 
				else
					res["r"]=sqrt(CGAL::squared_radius(p0,p1,p2));

			} if( info == "a") {
				NumericVector a(3);
				double area=CGAL_Delaunay2_cell_area(structure,v0);
				a[0]=(area>0 ? area : NA_REAL);
				area=CGAL_Delaunay2_cell_area(structure,v1);
				a[1]=(area>0 ? area : NA_REAL);
				area=CGAL_Delaunay2_cell_area(structure,v2);
				a[2]=(area>0 ? area : NA_REAL);
				res["a"]=a;
			}
			ret[i]=res;
		}

	}
	return ret;
}


template <>
List Del3TermType2D::update_infos(std::pair< HandleSet_Set,HandleSet_Set > sets) {
	return Del3TermType2D::update_infos(sets.first);
}

template <>
List Del3TermType2D::make_before_list() {
	HandleSet_Set conflictedFaces;
  	conflictedFaces=CGAL_Delaunay2_conflicted_faces(structure,current);
  	//prepare the negative list
  	//DEBUG: std::cout << "conflictedEdges=" << conflictedEdges.first.size() << std::endl;
  	return (locBefore=update_infos(conflictedFaces));
}	

template <>
List Del3TermType2D::make_after_list() {
	HandleSet_Set incidentFaces;
  	incidentFaces=CGAL_Delaunay2_incident_faces(structure,current_handle);
  	//prepare the positive list
  	//DEBUG: std::cout << "incidentEdges=" << incidentEdges.size() << std::endl;
  	return (locAfter=update_infos(incidentFaces));
}

template <> template <>
void Del3TermType2D::make_local_lists<INSERTION>() {
	//before INSERTION
	Del3TermType2D::make_before_list();

  	//INSERTION
  	apply_INSERTION();
  	
  	//after INSERTION
  	Del3TermType2D::make_after_list();
  	//as before
  	apply_DELETION();
}

template <> template <>
void Del3TermType2D::make_local_lists<DELETION>() {
	//before DELETION
  	 
  	Del3TermType2D::make_after_list();
  	//DELETION
  	apply_DELETION();
	//after DELETION
	 
  	Del3TermType2D::make_before_list();
  	//As before!
  	apply_INSERTION();
  	
}

template <> template<>
void Del3TermType2D::set_current_<INSERTION>(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_2(p[0],p[1]);
	//create the lists of edges
	if(auto_make_list) make_local_lists<INSERTION>();
}

template <> template<>
void Del3TermType2D::set_current_<DELETION>(NumericVector p) {
	double i=p[0];
	current_index=i;
	if(current_index <0 || current_index >= structure->number_of_vertices()) {
		//DEBUG: std::cout << "current_index=" << current_index ;
		//DEBUG: std::cout << " not in [0,"<<structure->number_of_vertices()-1<<"]" << std::endl;
		return;
	}
	//DEBUG: std::cout << "current_index=" << current_index <<std::endl;
	current_handle=Triangulation_vertex_at_pos<Delaunay2>(structure,current_index);
	
	current=current_handle->point();
	current_info=current_handle->info();
	//create the lists of edges
	if(auto_make_list) make_local_lists<DELETION>();
}

template <> 
NumericVector Del3TermType2D::get_current() {
	return NumericVector::create(current.x(),current.y());
}

///////////////////////////////////* 3D *//////////////////////////////////
//specialisation methods for Del1TermType3D : TODO
//specialisation methods for Del2TermType3D

template<>
List Del2TermType3D::update_infos(HandleSet_Set set) {
	int i=0;
	List ret(set.size());
	for(
		HandleSet_Set::iterator eit = set.begin();
		eit != set.end();
		++eit,++i
	) {
		Handle v0=*((*eit).begin()),v1=*((*eit).rbegin());
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

			} else if( info == "v") {
				res["v"]=List::create(v0->info(),v1->info());
			}
			ret[i]=res;
		}

	}
	return ret;
}

template<>
List Del2TermType3D::update_infos(std::pair< HandleSet_Set, HandleSet_Set> sets) {
	return Del2TermType3D::update_infos(sets.first);
}


template <>
List Del2TermType3D::make_before_list() {
	std::pair<HandleSet_Set,HandleSet_Set> conflictedEdges;
  	conflictedEdges=CGAL_Delaunay3_conflicted_and_boundary_edges(structure,current);
  	//prepare the negative list
  	return (locBefore=update_infos(conflictedEdges));
}	

template <>
List Del2TermType3D::make_after_list() {
	HandleSet_Set incidentEdges;
  	incidentEdges=CGAL_Delaunay3_incident_edges(structure,current_handle);
  	//prepare the positive list
  	return (locAfter=update_infos(incidentEdges));
}

template <> template <>
void Del2TermType3D::make_local_lists<INSERTION>() {
	//before INSERTION
	Del2TermType3D::make_before_list();

  	//INSERTION
  	apply_INSERTION();
  	
  	//after INSERTION
  	Del2TermType3D::make_after_list();
  	//as before
  	apply_DELETION();
}

template <> template <>
void Del2TermType3D::make_local_lists<DELETION>() {
	//before DELETION
  	 
  	Del2TermType3D::make_after_list();
  	//DELETION
  	apply_DELETION();
	//after DELETION
	 
  	Del2TermType3D::make_before_list();
  	//As before!
  	apply_INSERTION();
  	
}

template <> template<>
void Del2TermType3D::set_current_<INSERTION>(NumericVector p) {
	//check that structure is set!
	//TODO
	//set the current point
	current=Point_3(p[0],p[1],p[2]);
	//create the lists of edges
	if(auto_make_list)  make_local_lists<INSERTION>();
}

template <> template<>
void Del2TermType3D::set_current_<DELETION>(NumericVector p) {
	double i=p[0];
	current_index=i;
	if(current_index <0 || current_index >= structure->number_of_vertices()) return;
	//DEBUG: std::cout << "current_index=" << current_index <<std::endl;
	current_handle=Triangulation_vertex_at_pos<Delaunay3>(structure,current_index);
	current=current_handle->point();
	current_info=current_handle->info();
	//create the lists of edges
	if(auto_make_list) make_local_lists<DELETION>();
}

template <> 
NumericVector Del2TermType3D::get_current() {
	return NumericVector::create(current.x(),current.y(),current.z());
}

//specialisation methods for Del3TermType3D: (facets) TODO
//specialisation methods for Del4TermType3D: (cells) TODO

#endif //RCPP_TERM_DELAUNAY_H