#include "rcpp_delaunay_module.h"


RCPP_MODULE(delaunay_module) {
    class_<Del1TermType2D>( "Del1TermType2D" )
    .constructor()
    .field( "locBefore", &Del1TermType2D::locBefore, "local list before" )
    .field( "locAfter", &Del1TermType2D::locAfter, "local list after" )
    .field( "exprs.size", &Del1TermType2D::exprs_size, "exprs size" )
    .field( "cexprs.size", &Del1TermType2D::cexprs_size, "cexprs size" )
    //.property( "graph", &Del1TermType2D::get_struct, &Del1TermType2D::set_struct,"graph structure" )
    .property( "mode", &Del1TermType2D::get_mode, &Del1TermType2D::set_mode,
    "mode " )
    .property( "exprs", &Del1TermType2D::get_exprs, &Del1TermType2D::set_exprs,
    "exprs list" )
    .property( "cexprs", &Del1TermType2D::get_cexprs, &Del1TermType2D::set_cexprs,
    "common exprs list" )
    .property( "infos", &Del1TermType2D::get_infos, &Del1TermType2D::set_infos,
    "infos list" )
    .property( "params", &Del1TermType2D::get_params, &Del1TermType2D::set_params,
    "params list" )
    .method("get_envir",&Del1TermType2D::get_envir,"get envir")
    .method("set_struct",&Del1TermType2D::set_struct,"set struct")
    .method("get_struct",&Del1TermType2D::get_struct,"get struct")
    .method("set_current",&Del1TermType2D::set_current,"set point by coordinates (insertion) or index (suppression)")
    .method("get_current",&Del1TermType2D::get_current,"get point")
    .method("eval_first_expr",&Del1TermType2D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&Del1TermType2D::eval_exprs,"eval exprs")
    ;

	class_<Del2TermType2D>( "Del2TermType2D" )
	.constructor()
    .field( "locBefore", &Del2TermType2D::locBefore, "local list before" )
    .field( "locAfter", &Del2TermType2D::locAfter, "local list after" )
    .field( "exprs.size", &Del2TermType2D::exprs_size, "exprs size" )
    .field( "cexprs.size", &Del2TermType2D::cexprs_size, "cexprs size" )
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
    .method("get_envir",&Del2TermType2D::get_envir,"get envir")
    .method("set_struct",&Del2TermType2D::set_struct,"set struct")
    .method("get_struct",&Del2TermType2D::get_struct,"get struct")
    .method("set_current",&Del2TermType2D::set_current,"set point by coordinates (insertion) or index (suppression)")
    .method("get_current",&Del2TermType2D::get_current,"get point")
	.method("eval_first_expr",&Del2TermType2D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&Del2TermType2D::eval_exprs,"eval exprs")
	//.method("get_cexprs_caches",&Del2TermType2D::get_cexprs_caches,"get cexprs caches") //for debugging
    ;
	
	class_<Del2TermType3D>( "Del2TermType3D" )
	.constructor()
    .field( "locBefore", &Del2TermType3D::locBefore, "documentation for locBefore" )
    .field( "locAfter", &Del2TermType3D::locAfter, "documentation for locAfter" )
    .field( "exprs.size", &Del2TermType3D::exprs_size, "exprs size" )
    .field( "cexprs.size", &Del2TermType3D::cexprs_size, "cexprs size" )
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
    .method("set_struct",&Del2TermType3D::set_struct,"set struct")
    .method("get_struct",&Del2TermType3D::get_struct,"get struct")
    .method("set_current",&Del2TermType3D::set_current,"set point by coordinates (insertion) or index (suppression)")
    .method("get_current",&Del2TermType3D::get_current,"get point")
    .method("set_index",&Del2TermType3D::set_current_<DELETION>,"set (index) point to delete")
    .method("eval_first_expr",&Del2TermType3D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&Del2TermType3D::eval_exprs,"eval exprs")
	;

    class_<Interaction>("Interaction")
    .constructor<List>()
    .field( "single", &Interaction::single, "single parameter" )
    //.method("set_before_mode",&Interaction::set_before_mode,"lset before mode")
    .method("set_current",&Interaction::set_current,"set current")
    .method("local_energy",&Interaction::local_energy,"local energy")
    ;

    class_<ListsCache>("ListsCacheCpp")
    .constructor()
    .constructor<Interaction*,Domain*>()
    .constructor< List,std::vector<double>,std::vector<double> >()
    .field( "nb_runs", &ListsCache::nb_runs, "nb_runs" )
    .method("set_mode",&ListsCache::set_mode,"set mode")
    .method("mark_expr",&ListsCache::set_mark_expr,"set mark expr")
    .method("marked",&ListsCache::set_marked,"set marked")
    .method("new_mark",&ListsCache::new_mark,"new mark")
    .method("make_lists",&ListsCache::make_lists,"make lists cache")
    .method("get_lists",&ListsCache::get_lists,"get lists cache")
    .method("eval_exprs",&ListsCache::eval_exprs,"eval exprs")
    ;

    class_<Domain>("Domain")
    .constructor<std::vector<double>,std::vector<double> >()
    .method("pick",&Domain::pick,"")
    ;

    class_<SimGibbs>( "SimGibbsCpp" )
    .constructor()
    .constructor< List,std::vector<double>,std::vector<double> >()
    .field( "nb_runs", &SimGibbs::nb_runs, "nb_runs" )
    .property("single",&SimGibbs::get_single,&SimGibbs::set_single,"single")
    .method("mark_expr",&SimGibbs::set_mark_expr,"set mark expr")
    .method("marked",&SimGibbs::set_marked,"set marked")
    .method("new_mark",&SimGibbs::new_mark,"new mark")
    .method("run",&SimGibbs::run,"run")
    .method("get_domain",&SimGibbs::get_domain,"get domain")
    ;

}
