#ifndef RCPP_TERM_EXPRESSION_H
#define RCPP_TERM_EXPRESSION_H

#include <Rcpp.h>
#include "cgal_spatstat_triangulation.h"
using namespace Rcpp ;

//Register interaction type here!!!
enum InterTypeID {
    //Point Process
    DEL1=0, DEL2, DEL3, DEL4, ALL1, ALL2
};

//ID: type of interaction, POINT: class Point, DIM: dimension, ORDER: graph order 
template <InterTypeID ID,class POINT,int DIM=2,int ORDER=1>
class TermType { 

	public:
    TermType() {
    	Environment envir=Environment::global_env().new_child(true);
    }

    void set_exprs(List exprs_) { exprs=exprs_; }
    List get_exprs() { return exprs; }

    void set_cexprs(List cexprs_) { cexprs=cexprs_; }
    List get_cexprs() { return cexprs; }

    void set_infos(CharacterVector infos_) { infos=infos_; }
    CharacterVector get_infos() { return infos; }

    void set_params(List params_) {
    	params=params_;
    	CharacterVector names=params.names();
    	int l=params.size();

	    for(int i = 0; i < l ; i++) {
			SEXP name = Rf_install(Rf_translateChar(STRING_ELT(names, i)));
			Rf_defineVar(name, VECTOR_ELT(params, i), envir);
	    }
    }
    List get_params() {return params;}

    void set_point_to_insert(NumericVector p);

    void set_point_to_remove(IntegerVector rank);

    NumericVector get_point();

    void update_infos();

    double eval_exprs();
     
	private:
        POINT point;
		//exprs: list of Language
		List exprs;	
		//cexprs : named list of Language
		// cexprs stand for common exprs which are repeated several times
		// cexprs is the information to save for each point 
		List cexprs;
		//infos: Vector of infos
		// an info is a predefined quantity depending on the TermEnergyType
		CharacterVector infos;
		// list of local contributions (before and after) and global contributions
		List locBefore,locAfter,glob;
		//params
		List params;
		//Environment
		Environment envir;
};

#endif //RCPP_TERM_EXPRESSION_H