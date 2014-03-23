#include "rcpp_term_expr.h"

typedef TermType<DEL2,Delaunay2,Point_2,2> Del2TermType2D;
typedef TermType<DEL2,Delaunay3,Point_3,3> Del2TermType3D;

//specialisation method
template <> 
void Del2TermType2D::set_current(NumericVector p) {
	current=Point_2(p[0],p[1]);
}

template <> 
void Del2TermType2D::set_current_index(int rank) {
	
}

template <> 
NumericVector Del2TermType2D::get_current() {
	return NumericVector::create(current.x(),current.y());
}

template <> 
void Del2TermType3D::set_current(NumericVector p) {
	current=Point_3(p[0],p[1],p[2]);
}

template <> 
NumericVector Del2TermType3D::get_current() {
	return NumericVector::create(current.x(),current.y(),current.z());
}