#include "rcpp_term_delaunay.h"

RCPP_MODULE(term_module) {
	class_<Del2TermType2D>( "Del2TermType2D" )
	.constructor()
	.property( "exprs", &Del2TermType2D::get_exprs, &Del2TermType2D::set_exprs,
    "exprs list" )
    .property( "cexprs", &Del2TermType2D::get_cexprs, &Del2TermType2D::set_cexprs,
    "common exprs list" )
    .property( "infos", &Del2TermType2D::get_infos, &Del2TermType2D::set_infos,
    "infos list" )
    .property( "params", &Del2TermType2D::get_params, &Del2TermType2D::set_params,
    "params list" )
    .method("set_point_to_insert",&Del2TermType2D::set_point_to_insert,"set point to insert")
    .method("get_point",&Del2TermType2D::get_point,"get point")
	//.method("eval_exprs",&Del2EnergyType2D::eval_exprs_for,"eval exprs for a specified point")
	;
	
	class_<Del2TermType3D>( "Del2TermType3D" )
	.constructor()
	.property( "exprs", &Del2TermType3D::get_exprs, &Del2TermType3D::set_exprs,
    "exprs list" )
    .property( "cexprs", &Del2TermType3D::get_cexprs, &Del2TermType3D::set_cexprs,
    "common exprs list" )
    .property( "infos", &Del2TermType3D::get_infos, &Del2TermType3D::set_infos,
    "infos list" )
    .property( "params", &Del2TermType3D::get_params, &Del2TermType3D::set_params,
    "params list" )
    .method("set_point_to_insert",&Del2TermType3D::set_point_to_insert,"set point to insert")
    .method("get_point",&Del2TermType3D::get_point,"get point")
	//.method("eval_exprs",&Del2EnergyType2D::eval_exprs_for,"eval exprs for a specified point")
	;
}
