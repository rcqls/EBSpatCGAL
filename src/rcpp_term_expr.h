#ifndef RCPP_TERM_EXPRESSION_H
#define RCPP_TERM_EXPRESSION_H

#include "rcpp_spatstat_triangulation.h"
using namespace Rcpp ;

//Register interaction type here!!!
enum InterTypeID {
    //Point Process
    DEL1=0, DEL2, DEL3, DEL4, ALL1, ALL2
};

enum TermMode {INS=0,SUPPR};

//STRUCT: class of the structure (ex: Delaunay2) , ELEMENT: current element (ex: Point_2)
//ID: type of interaction, DIM: dimension, ORDER: structure order if needed (ex: ORDER=2 for Delaunay) 
//CONTAINER: class needed to update infos
template <InterTypeID ID,class STRUCT, class ELEMENT, class CONTAINER,int DIM=2, int ORDER=1>
class TermType { 

	public:
    TermType() {
    	Environment envir=Environment::global_env().new_child(true);
    }

    void set_struct(STRUCT struct_) {structure=struct_;}

    STRUCT get_struct() {return structure;}

    void set_exprs(List exprs_) { exprs=exprs_; }
    List get_exprs() { return exprs; }

    void set_cexprs(List cexprs_) { cexprs=cexprs_; }
    List get_cexprs() { return cexprs; }

    void set_infos(std::vector< std::string > infos_) { infos=infos_; }
    std::vector< std::string > get_infos() { return infos; }

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

    void set_mode(int mode_) {mode=static_cast<TermMode>(mode_);}

    int get_mode() {return static_cast<int>(mode);}

    void set_current(NumericVector p); //by coordinates

    void set_current_index(int rank); //by index

    NumericVector get_current();

    int get_current_index();

    double eval_exprs();

    double eval_single_expr(Language expr);

    List update_infos(CONTAINER set);
     
	private:
        //
        STRUCT structure;
        //Term mode (ex: INS for insertion) 
        TermMode mode;
        //Current element
        ELEMENT current;
        //Index of the current element
        int currentIndex;
		//exprs: list of Language
		List exprs;	
		//cexprs : named list of Language
		// cexprs stand for common exprs which are repeated several times
		// cexprs is the information to save for each point 
		List cexprs;
		//infos: Vector of infos
		// an info is a predefined quantity depending on the TermEnergyType
		std::vector< std::string > infos;
		// list of local contributions (before and after) and global contributions
		List locBefore,locAfter,glob;
		//params
		List params;
		//Environment
		Environment envir;

        void make_local_lists();
        void make_global_list();
};

#endif //RCPP_TERM_EXPRESSION_H