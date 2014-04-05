#ifndef RCPP_DOM_H
#define RCPP_DOM_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

template <typename STRUCT> //, typename ELEMENT = typename STRUCT::Point, typename HANDLE = typename STRUCT::Vertex_handle>
class Domain {
public: 
    Domain() {}; //needed by rcpp_delaunay_module.cpp
    Domain(STRUCT* structure_,std::vector<double> left_,std::vector<double> right_) {//Rectangle first
        structure=structure_;
        left=left_;right=right_;
        dim=left.size();
        //DEBUG: std::cout << "dim="<< dim << std::endl;
        inside_number=structure->number_of_vertices();
        set_size();
    };

    NumericVector propose_INSERTION() {
        double a,b;
        std::vector<double> newPt(dim);
        for(int i=0;i<dim;i++) newPt[i]=as<double>(runif(1,left[i],right[i]));
        return NumericVector(newPt.begin(),newPt.end());
    };

    NumericVector propose_DELETION() {
        return NumericVector::create(as<double>(runif(1,0,inside_number)));
        
    };

    STRUCT* get_struct() {return structure;}

    std::vector<double> left,right; //in some axe
    
    //maintain number of inside domain point
    int inside_number;

    double get_size() {return size;} //area or volume

    int get_dim() {return dim;}
protected:
    STRUCT* structure;

    int dim;

    double size;

    void set_size() {
        size=1;
        for(int i=0;i<dim;i++) size *=right[i]-left[i];
    }

    // void update_inside_number() {
    //     for(HANDLE::iterator hit=structure->begin_vertices())
    // }
};
#endif //RCPP_DOM_H