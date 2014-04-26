#ifndef RCPP_LISTS_CACHE_H
#define RCPP_LISTS_CACHE_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

enum CacheMode {Systematic=0,Random,Existing};

//TODO: remove the dependency on STRUCT since Interaction via first TermType knows everything about STRUCT (like SimGibbs)
//template <typename STRUCT> //, typename ELEMENT = typename STRUCT::Point, typename HANDLE = typename STRUCT::Vertex_handle>
class ListsCache {
public: 
    ListsCache() {set_envir();nb_runs=10000;}; //needed by rcpp_delaunay_module.cpp
    ListsCache(Interaction* inter_,Domain* domain_) {
        domain=domain_;
        inter=inter_;
        set_envir();
        nb_runs=10000; //MC approach
    };

    ListsCache(List inter_,std::vector<double> left_,std::vector<double> right_) {
        set_interaction(inter_);
        set_domain(left_,right_);
        set_envir();
        nb_runs=10000; //MC approach
    }

    void set_interaction(List inter_) {
        inter=new Interaction(inter_);
    }

    // Indirectly set the mark expr of Interaction.
    // This is because Interaction is not set in R directly.
    // Same trick inside SimGibbs.
    void set_mark_expr(Language expr) {
        inter->set_mark_expr(expr);
    }

    void set_marked(bool state) {marked=state;}

    List new_mark() {return inter->new_mark();}


    void set_domain(Domain* domain_) {
        domain=domain_;
    }

    void set_domain(std::vector<double> left_,std::vector<double> right_) {
        domain=new Domain(left_,right_);
    }

    Domain* get_domain() {return domain;}

    //TODO: not adapted to a real domain
    void init_inside_number() {
        inside_number=inter->inside_number(domain);
    };

    void set_mode(int mode_) {mode=static_cast<CacheMode>(mode_);};

    int get_mode() {return static_cast<int>(mode);};

    // void set_size(NumericVector size_) {//user is in charge of the proper dimension
    // 	int i=0;
    // 	size=size_;
    // 	delta.reserve(size.size());

    // 	for(
    // 		std::vector<int>::iterator it=size.begin();
    // 		it!=size.end();
    // 		++it,++i
    // 	) {
    // 		delta[i]=(domain->right[i]-domain->left[i])/size[i];
    // 	}
    // } 

    void init() {
    	lists.erase(lists.begin(),lists.end());
    };

    void make_lists() {
    	switch(mode) {
    	case Systematic:
    		lists=List::create(1);
    		lists[0]=NumericVector::create(3);
    		break; 
    	case Random:
            lists=List(nb_runs);
            for(int i=0;i<nb_runs;i++) {
                pick_new();
                //inter->make_local_lists();  
                lists[i]=inter->get_cexprs();
            }
    		break;
    	case Existing: //No stress there is no optimization consideration!
            init_inside_number();
    		lists=List(inside_number);
    		for(int i=0;i<inside_number;i++) {//TODO: inside_list_index
    			//std::cout << "i=" << i << std::endl;
    			inter->set_current(NumericVector::create(i));//TODO
    			//inter->make_local_lists();	
    			lists[i]=inter->get_cexprs();
    		};
    		//lists["test"]=NumericVector::create(i);
    	};
    	//std::cout << "end" << std::endl;
    };

    List get_lists() {return lists;};

    // 
    void set_envir() {
        envir=Environment::global_env().new_child(true);
    }

    Environment get_envir() {return envir;}

    int nb_runs;

protected:
	Domain* domain;
	//STRUCT* structure;
	std::vector<int> size,delta;
	Interaction* inter;
	List lists; //
	CacheMode mode;
    bool marked;
    int inside_number;

    Environment envir;

    void import_vars(List vars_) {
        CharacterVector names=vars_.names();
        int l=vars_.size();

        for(int i = 0; i < l ; i++) {
            SEXP name = Rf_install(Rf_translateChar(STRING_ELT(names, i)));
            Rf_defineVar(name, VECTOR_ELT(vars_, i), envir);
        }
    }


    void pick_new() { //equivalent of propose_INSERTION() for SimGibbs
        inter -> set_current(domain->pick());
        if(marked) inter->set_current_mark();
    }

    


};

#endif //RCPP_LISTS_CACHE_H