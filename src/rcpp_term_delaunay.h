#include "rcpp_term_expr.h"

typedef TermType<DEL2,Point_2,2> Del2TermType2D;
typedef TermType<DEL2,Point_3,3> Del2TermType3D;

//specialisation method
template <> 
void Del2TermType2D::set_point_to_insert(NumericVector p) {
	point=Point_2(p[0],p[1]);
}

template <> 
NumericVector Del2TermType2D::get_point() {
	return NumericVector::create(point.x(),point.y());
}

template <> 
void Del2TermType3D::set_point_to_insert(NumericVector p) {
	point=Point_3(p[0],p[1],p[2]);
}

template <> 
NumericVector Del2TermType3D::get_point() {
	return NumericVector::create(point.x(),point.y(),point.z());
}