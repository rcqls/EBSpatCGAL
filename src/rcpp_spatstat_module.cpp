#include "rcpp_term_delaunay.h"
#include "rcpp_sim_delaunay.h"


RCPP_MODULE(spatstat_module) {
	class_<Del2TermType2D>( "Del2TermType2D" )
	.constructor()
    .field( "locBefore", &Del2TermType2D::locBefore, "local list before" )
    .field( "locAfter", &Del2TermType2D::locAfter, "locol list after" )
    .field( "exprs.size", &Del2TermType2D::exprs_size, "exprs size" )
    .field( "cexprs.size", &Del2TermType2D::cexprs_size, "cexprs size" )
    //.field( "mode.as.before", &Del2TermType2D::mode_as_before, "mode as before" )
    //.property( "graph", &Del2TermType2D::get_struct, &Del2TermType2D::set_struct,"graph structure" )
    .property( "mode", &Del2TermType2D::get_mode, &Del2TermType2D::set_mode,
    "mode " )
	.property( "exprs", &Del2TermType2D::get_exprs, &Del2TermType2D::set_exprs,
    "exprs list" )
    .property( "cexprs", &Del2TermType2D::get_cexprs, &Del2TermType2D::set_cexprs,
    "common exprs list" )
    .property( "infos", &Del2TermType2D::get_infos, &Del2TermType2D::set_infos,
    "infos list" )
    .property( "params", &Del2TermType2D::get_params, &Del2TermType2D::set_params,
    "params list" )
    .method("set_graph",&Del2TermType2D::set_struct,"set graph")
    .method("get_graph",&Del2TermType2D::get_struct,"get graph")
    .method("set_point",&Del2TermType2D::set_current<INSERTION>,"set point to insert")
    .method("get_point",&Del2TermType2D::get_current,"get point")
    .method("set_index",&Del2TermType2D::set_current<DELETION>,"set (index) point to delete")
	.method("eval_first_expr",&Del2TermType2D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&Del2TermType2D::eval_exprs,"eval exprs")
	;
	
	class_<Del2TermType3D>( "Del2TermType3D" )
	.constructor()
    .field( "locBefore", &Del2TermType3D::locBefore, "documentation for locBefore" )
    .field( "locAfter", &Del2TermType3D::locAfter, "documentation for locAfter" )
    .field( "exprs.size", &Del2TermType3D::exprs_size, "exprs size" )
    .field( "cexprs.size", &Del2TermType3D::cexprs_size, "cexprs size" )
    //.field( "mode.as.before", &Del2TermType3D::mode_as_before, "mode as before" )
    //.property( "graph", &Del2TermType3D::get_struct, &Del2TermType3D::set_struct,"graph structure" )
    .property( "mode", &Del2TermType3D::get_mode, &Del2TermType3D::set_mode,
    "mode " )
	.property( "exprs", &Del2TermType3D::get_exprs, &Del2TermType3D::set_exprs,
    "exprs list" )
    .property( "cexprs", &Del2TermType3D::get_cexprs, &Del2TermType3D::set_cexprs,
    "common exprs list" )
    .property( "infos", &Del2TermType3D::get_infos, &Del2TermType3D::set_infos,
    "infos list" )
    .property( "params", &Del2TermType3D::get_params, &Del2TermType3D::set_params,
    "params list" )
    .method("set_graph",&Del2TermType3D::set_struct,"set graph")
    .method("get_graph",&Del2TermType3D::get_struct,"get graph")
    .method("set_point",&Del2TermType3D::set_current<INSERTION>,"set point to insert")
    .method("get_point",&Del2TermType3D::get_current,"get point")
    .method("set_index",&Del2TermType3D::set_current<DELETION>,"set (index) point to delete")
    .method("eval_first_expr",&Del2TermType3D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&Del2TermType3D::eval_exprs,"eval exprs")
	;

    class_<Interaction>("Interaction")
    .constructor<List>()
    .field( "single", &Interaction::single, "single parameter" )
    //.method("set_before_mode",&Interaction::set_before_mode,"lset before mode")
    .method("set_point",&Interaction::set_point,"set point")
    .method("local_energy",&Interaction::local_energy,"local energy")
    ;

    class_<Del2SimGibbs2D>( "Del2SimGibbs2D" )
    .constructor<List>()
    .field( "nb_runs", &Del2SimGibbs2D::nb_runs, "nb_runs" )
    .method("run",&Del2SimGibbs2D::run,"run")
    ;

    class_<Del2SimGibbs3D>( "Del2SimGibbs3D" )
    .constructor<List>()
    .field( "nb_runs", &Del2SimGibbs3D::nb_runs, "nb_runs" )
    .method("run",&Del2SimGibbs3D::run,"run")
    ;
}
