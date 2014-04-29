#ifndef RCPP_LISTS_CACHE_H
#define RCPP_LISTS_CACHE_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

enum CacheMode {Systematic=0,Random};

//TODO: remove the dependency on STRUCT since Interaction via first TermType knows everything about STRUCT (like SimGibbs)
//template <typename STRUCT> //, typename ELEMENT = typename STRUCT::Point, typename HANDLE = typename STRUCT::Vertex_handle>
class ListsCache {
public: 
    ListsCache() {set_mode(1);nb_runs=1000;}; //needed by rcpp_delaunay_module.cpp
    ListsCache(Interaction* inter_,Domain* domain_) {
        domain=domain_;
        inter=inter_;
        set_mode(1);
        nb_runs=1000; //MC approach
    };

    ListsCache(List inter_,std::vector<double> left_,std::vector<double> right_) {
        set_interaction(inter_);
        set_domain(left_,right_);
        set_mode(1);
        nb_runs=1000; //MC approach
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

    void make_lists() {
        //First list
    	switch(mode) {
    	case Systematic:
    		first_list=List::create(1);
    		first_list[0]=NumericVector::create(3);
    		break; 
    	case Random:
            first_list=List(nb_runs);
            for(int i=0;i<nb_runs;i++) {
                pick_new();
                //inter->make_local_lists();  
                first_list[i]=inter->get_cexprs();
            }
    	};

        // Second list
    	//No stress there is no optimization consideration!
        init_inside_number();
		second_list=List(inside_number);
		for(int i=0;i<inside_number;i++) {//TODO: inside_list_index
			//std::cout << "i=" << i << std::endl;
			inter->set_current(NumericVector::create(i));//TODO
			//inter->make_local_lists();	
			second_list[i]=inter->get_cexprs();
		};
		//lists["test"]=NumericVector::create(i);
    	 
    	//std::cout << "end" << std::endl;
        inter->set_exprs_cexprs_caches(get_lists_to_send());
    };

    List eval_exprs() {
        return inter->eval_exprs_from_cexprs_caches();
    }

    List get_lists() {return List::create(_["first"]=first_list,_["second"]=second_list);};

    List get_lists_to_send() {return List::create(_["first"]=prepare_list_to_send(first_list),_["second"]=prepare_list_to_send(second_list));};

    //prepare lists to be used in Interaction
    List prepare_list_to_send(List cl) {
        //This can be done directly in c++
        List res;
        if(cl.size()>0) {
            List cl0=cl[0];
            res=List(cl0.size());
            for(int term=0;term < cl0.size();term++) {
                List resBefore,resAfter;
                
                for(int i=0;i<cl.size();i++) {
                    List clTerm=as<List>(cl[i])[term];
                    List clBefore=clTerm["before"],clAfter=clTerm["after"];
                    for(int j=0;j<clBefore.size();j++) resBefore.push_back(clBefore[j]);
                    for(int j=0;j<clAfter.size();j++) resAfter.push_back(clAfter[j]);
                }
                res[term]=List::create(_["before"]=resBefore,_["after"]=resAfter);
            }

          }  
          return res;
    }

    int nb_runs;

protected:
	Domain* domain;
	//STRUCT* structure;
	std::vector<int> size,delta;
	Interaction* inter;
	List first_list,second_list; //
	CacheMode mode;
    bool marked;
    int inside_number;

    void pick_new() { //equivalent of propose_INSERTION() for SimGibbs
        inter -> set_current(domain->pick());
        if(marked) inter->set_current_mark();
    }

    


};

#endif //RCPP_LISTS_CACHE_H