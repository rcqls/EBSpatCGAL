#ifndef RCPP_GNZ_CACHE_H
#define RCPP_GNZ_CACHE_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

enum CacheMode {Systematic=0,Random};

//TODO: remove the dependency on STRUCT since Interaction via first TermType knows everything about STRUCT (like SimGibbs)
//template <typename STRUCT> //, typename ELEMENT = typename STRUCT::Point, typename HANDLE = typename STRUCT::Vertex_handle>
class GNZCache {
public: 
    GNZCache() {set_mode(1);nb_runs=1000;}; //needed by rcpp_delaunay_module.cpp
    
    // GNZCache(Interaction* inter_,Domain* domain_) {
    //     domain=domain_;
    //     inter=inter_;
    //     set_mode(1);
    //     nb_runs=1000; //MC approach
    // };

    GNZCache(List inter_,Domain* domain_) {
        set_interaction(inter_);
        domain=domain_;
        set_mode(1);
        nb_runs=1000; //MC approach
    }

    // GNZCache(List inter_,std::vector<double> left_,std::vector<double> right_) {
    //     set_interaction(inter_);
    //     set_domain_double(left_,right_);
    //     set_mode(1);
    //     nb_runs=1000; //MC approach
    // }

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

    // void set_domain_double(std::vector<double> left_,std::vector<double> right_) {
    //     domain=new Domain(left_,right_);
    // }

    Domain* get_domain() {return domain;}

    //TODO: not adapted to a real domain
    void init_inside_number() {
        inside_number=inter->inside_number(domain);
        inside_indexes=inter->inside_indexes(domain,inside_number);
    };

    int get_inside_number() {return inside_number;}

    IntegerVector get_inside_indexes() {return inside_indexes;}

    void set_mode(int mode_) {mode=static_cast<CacheMode>(mode_);};

    int get_mode() {return static_cast<int>(mode);};

    void make_lists() {
        init_inside_number();
        //First list
    	switch(mode) {
        	case Systematic: {
                set_sizes_for_interaction(IntegerVector::create(domain->grid_length,inside_number));

        		first_list=List(domain->grid_length);
                int i=0;
                //std::cout << "grid_delta[0]" << domain->grid_delta[0] << std::endl;
                //std::cout << "grid_delta[1]" << domain->grid_delta[1] << std::endl;
                //std::cout << "grid_delta[2]" << domain->grid_delta[2] << std::endl;
                for(
                    double x=domain->get_left(0)+domain->grid_delta[0]/2.0;
                    x <= domain->get_right(0);
                    x += domain->grid_delta[0]
                ) for(
                    double y=domain->get_left(1)+domain->grid_delta[1]/2.0;
                    y <= domain->get_right(1);
                    y += domain->grid_delta[1]
                ) for(
                    double z= domain->get_left(2)+domain->grid_delta[2]/2.0;
                    z <= domain->get_right(2);
                    z += domain->grid_delta[2]
                ) {
                    NumericVector ptGrid;
                    //std::cout << "i,x,y,z=" << i << "," << x << "," << y << ","<< z << std::endl;
                    ptGrid = domain->get_dim()==2 ? NumericVector::create(x,y) : NumericVector::create(x,y,z);

                    inter -> set_current(ptGrid);
                    if(marked) inter->set_current_mark();
                    first_list[i++]=inter->get_cexprs();
                }

        		break; 
            }
        	case Random: {
                set_sizes_for_interaction(IntegerVector::create(nb_runs,inside_number));

                first_list=List(nb_runs);
                for(int i=0;i<nb_runs;i++) {
                    pick_new();
                    //inter->make_local_lists();  
                    first_list[i]=inter->get_cexprs();
                }
            }
    	};

        // Second list
    	//No stress there is no optimization consideration!
		second_list=List(inside_number);
        int i=0;
		for(
            IntegerVector::iterator it=inside_indexes.begin();
            it!= inside_indexes.end();
            ++it,i++
        ) {//TODO: inside_list_index
			//std::cout << "i=" << i  << " & it=" << *it << std::endl;
			inter->set_current(NumericVector::create(*it)); //TODO
			//inter->make_local_lists();	
			second_list[i]=inter->get_cexprs();
		};
		//lists["test"]=NumericVector::create(i);
    	 
    	//std::cout << "end" << std::endl;
        inter->set_cexprs_caches(get_lists_to_send());
    };

    void set_exprs_for_interaction(List caches_exprs) {
        inter->set_exprs_for_caches(caches_exprs ); 
    }

    void set_sizes_for_interaction(IntegerVector caches_sizes) {
        inter->set_sizes_for_caches(caches_sizes); 
    }

    void set_single(NumericVector single) {inter-> set_single(single);}

    NumericVector get_single() {return inter->get_single();}

    NumericVector eval_first_exprs(std::vector<int> indexes) {
        return inter->eval_first_exprs_from_cexprs_caches(indexes);
    }

    NumericVector eval_second_exprs(std::vector<int> indexes) {
        return inter->eval_second_exprs_from_cexprs_caches(indexes);
    }

    List eval_exprs() {
        return  inter->eval_exprs_from_cexprs_caches();
    }

    Environment get_envir() {return inter->envir;}

    List get_cexprs_lists() {return List::create(_["first"]=first_list,_["second"]=second_list);};

    List get_exprs_lists() {return inter->get_exprs_from_cexprs_caches();}

    List get_lists_to_send() {return List::create(_["first"]=prepare_list_to_send(first_list),_["second"]=prepare_list_to_send(second_list));};

    //prepare lists to be used in Interaction
    List prepare_list_to_send(List cl) {
        //This can be done directly in c++
        List res;
        if(cl.size()>0) {
            List cl0=cl[0];
            res=List(cl0.size());
            for(int term=0;term < cl0.size();term++) {
                List resTerm(cl.size());
                
                for(int i=0;i<cl.size();i++) {
                    resTerm[i]=as<List>(cl[i])[term];     
                }
                res[term]=resTerm;
            }

        }  
        return res;
    }

    /*************
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
    } ******************/

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
    IntegerVector inside_indexes;

    void pick_new() { //equivalent of propose_INSERTION() for SimGibbs
        inter -> set_current(domain->pick());
        if(marked) inter->set_current_mark();
    }


};

#endif //RCPP_GNZ_CACHE_H