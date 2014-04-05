#ifndef RCPP_LISTS_CACHE_H
#define RCPP_LISTS_CACHE_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

enum CacheMode {Systematic=0,Random,Existing};

template <typename STRUCT> //, typename ELEMENT = typename STRUCT::Point, typename HANDLE = typename STRUCT::Vertex_handle>
class ListsCache {
public: 
    ListsCache() {}; //needed by rcpp_delaunay_module.cpp
    ListsCache(Domain<STRUCT>* domain_,Interaction* inter_) {
        domain=domain_;
        structure=domain->get_struct();
        inter=inter_;
    };

    void set_mode(int mode_) {mode=static_cast<CacheMode>(mode_);};

    int get_mode() {return static_cast<int>(mode);};

    void set_size(NumericVector size_) {//user is in charge of the proper dimension
    	int i=0;
    	size=size_;
    	delta.reserve(size.size());

    	for(
    		std::vector<int>::iterator it=size.begin();
    		it!=size.end();
    		++it,++i
    	) {
    		delta[i]=(domain->right[i]-domain->left[i])/size[i];
    	}
    } 

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
    		break;
    	case Existing: //No stress there is no optimization consideration!

    		lists=List(structure->number_of_vertices());
    		for(int i=0;i<structure->number_of_vertices();i++) {
    			std::cout << "i=" << i << std::endl;
    			inter->set_current(NumericVector::create(i));
    			inter->make_local_lists();	
    			lists[i]=inter->get_local_lists();
    		};
    		//lists["test"]=NumericVector::create(i);
    	};
    	std::cout << "end" << std::endl;
    };

    List get_lists() {return lists;};

protected:
	Domain<STRUCT>* domain;
	STRUCT* structure;
	std::vector<int> size,delta;
	Interaction* inter;
	List lists; //
	CacheMode mode;

};

#endif //RCPP_LISTS_CACHE_H