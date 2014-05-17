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
    .property("args",&Del1TermType2D::get_args, &Del1TermType2D::set_args,
    "args setting")
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
    .property("args",&Del2TermType2D::get_args, &Del2TermType2D::set_args,
    "args setting")
    .property( "params", &Del2TermType2D::get_params, &Del2TermType2D::set_params,
    "params list" )
    .method("get_envir",&Del2TermType2D::get_envir,"get envir")
    .method("set_struct",&Del2TermType2D::set_struct,"set struct")
    .method("get_struct",&Del2TermType2D::get_struct,"get struct")
    .method("set_current",&Del2TermType2D::set_current,"set point by coordinates (insertion) or index (suppression)")
    .method("get_current",&Del2TermType2D::get_current,"get point")
	.method("eval_first_expr",&Del2TermType2D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&Del2TermType2D::eval_exprs,"eval exprs")
	.method("get_cexprs_caches",&Del2TermType2D::get_cexprs_caches,"get cexprs caches") //for debugging
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
    .property("args",&Del2TermType3D::get_args, &Del2TermType3D::set_args,
    "args setting")
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

    class_<All2TermType2D>( "All2TermType2D" )
    .constructor()
    .field( "locBefore", &All2TermType2D::locBefore, "local list before" )
    .field( "locAfter", &All2TermType2D::locAfter, "local list after" )
    .field( "exprs.size", &All2TermType2D::exprs_size, "exprs size" )
    .field( "cexprs.size", &All2TermType2D::cexprs_size, "cexprs size" )
    //.property( "graph", &All2TermType2D::get_struct, &All2TermType2D::set_struct,"graph structure" )
    .property( "mode", &All2TermType2D::get_mode, &All2TermType2D::set_mode,
    "mode " )
    .property( "exprs", &All2TermType2D::get_exprs, &All2TermType2D::set_exprs,
    "exprs list" )
    .property( "cexprs", &All2TermType2D::get_cexprs, &All2TermType2D::set_cexprs,
    "common exprs list" )
    .property( "infos", &All2TermType2D::get_infos, &All2TermType2D::set_infos,
    "infos list" )
    .property("args",&All2TermType2D::get_args, &All2TermType2D::set_args,
    "args setting")
    .property( "params", &All2TermType2D::get_params, &All2TermType2D::set_params,
    "params list" )
    .method("get_envir",&All2TermType2D::get_envir,"get envir")
    .method("set_struct",&All2TermType2D::set_struct,"set struct")
    .method("get_struct",&All2TermType2D::get_struct,"get struct")
    .method("set_current",&All2TermType2D::set_current,"set point by coordinates (insertion) or index (suppression)")
    .method("get_current",&All2TermType2D::get_current,"get point")
    .method("eval_first_expr",&All2TermType2D::eval_first_expr,"eval first expr")
    .method("eval_exprs",&All2TermType2D::eval_exprs,"eval exprs")
    .method("get_cexprs_caches",&All2TermType2D::get_cexprs_caches,"get cexprs caches") //for debugging
    ;

    class_<Interaction>("Interaction")
    .constructor<List>()
    .field( "single", &Interaction::single, "single parameter" )
    //.method("set_before_mode",&Interaction::set_before_mode,"lset before mode")
    .method("set_current",&Interaction::set_current,"set current")
    .method("local_energy",&Interaction::local_energy,"local energy")
    ;

    class_<Domain>("DomainCpp")
    .constructor<std::vector<double>,std::vector<double> >()
    .field( "left", &Domain::left, "left" )
    .field( "right", &Domain::right, "right" )
    .field( "length_grid", &Domain::grid_length, "grid length" )
    .field( "delta_grid", &Domain::grid_delta, "grid delta" )
    .field( "size_grid", &Domain::grid_size, "grid size" )
    .method("pick",&Domain::pick,"pick a point")
    .method("contains",&Domain::contains_,"contains")
    .method("get_dim",&Domain::get_dim,"get dim")
    .method("get_size",&Domain::get_size,"get size")
    .method("set_grid",&Domain::set_grid,"set grid")
    ;

    class_<GNZCache>("GNZCacheCpp")
    .constructor()
    //.constructor<Interaction*,Domain*>()
    .constructor< List,Domain* >()
    //.constructor< List,std::vector<double>,std::vector<double> >()
    .property("single",&GNZCache::get_single,&GNZCache::set_single,"single")
    .field( "nb_runs", &GNZCache::nb_runs, "nb_runs" )
    .method("get_envir",&GNZCache::get_envir,"get envir")
    .method("get_inside_indexes",&GNZCache::get_inside_indexes,"get indexes inside domain")
    .method("get_inside_number",&GNZCache::get_inside_number,"get number of indexes inside domain")
    .method("get_mode",&GNZCache::get_mode,"get mode")
    .method("set_mode",&GNZCache::set_mode,"set mode")
    .method("mark_expr",&GNZCache::set_mark_expr,"set mark expr")
    .method("marked",&GNZCache::set_marked,"set marked")
    .method("new_mark",&GNZCache::new_mark,"new mark")
    .method("make_lists",&GNZCache::make_lists,"make lists cache")
    //.method("set_single",&GNZCache::set_single,"set single")
    .method("set_exprs_for_interaction",&GNZCache::set_exprs_for_interaction,"set caches exprs for interaction")
    .method("set_sizes_for_interaction",&GNZCache::set_sizes_for_interaction,"set caches sizes for interaction")
    .method("set_domain",&GNZCache::set_domain,"set domain")
    .method("get_domain",&GNZCache::get_domain,"get domain")
    .method("get_cexprs_lists",&GNZCache::get_cexprs_lists,"get cexprs lists cache")
    .method("get_exprs_lists",&GNZCache::get_exprs_lists,"get exprs lists")
    .method("eval_first_exprs",&GNZCache::eval_first_exprs,"eval first exprs")
    .method("eval_second_exprs",&GNZCache::eval_second_exprs,"eval second exprs")
    .method("eval_exprs",&GNZCache::eval_exprs,"eval exprs")
    ;

    class_<SimGibbs>( "SimGibbsCpp" )
    .constructor()
    //.constructor< List,std::vector<double>,std::vector<double> >()
    .constructor< List,Domain* >()
    .field( "nb_runs", &SimGibbs::nb_runs, "nb_runs" )
    .property("single",&SimGibbs::get_single,&SimGibbs::set_single,"single")
    .method("mark_expr",&SimGibbs::set_mark_expr,"set mark expr")
    .method("marked",&SimGibbs::set_marked,"set marked")
    .method("new_mark",&SimGibbs::new_mark,"new mark")
    .method("run",&SimGibbs::run,"run")
    .method("set_domain",&SimGibbs::set_domain,"set domain")
    .method("get_domain",&SimGibbs::get_domain,"get domain")
    ;

}
