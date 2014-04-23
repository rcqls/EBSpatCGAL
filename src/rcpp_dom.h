#ifndef RCPP_DOM_H
#define RCPP_DOM_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

class Domain {
public: 
    // TODO : completer en spécifiant 4 types de domaines : 
    // Rectangulaires, Couronnes rectangulaires, Ellipsoidales et Couronnes ellipsoidales.
    // left,right délimitent contour extérieur quand left_in, right_in délimitent le contour intérieur
    // introduire mode= 1 (Ellipse) ou 0 (Rectangulaire)
    // Créer les méthodes pour:
    // 0) tester l'appartenance au domaine
    // 1) initialiser la liste des vertex_handle dans domaine 
    // 2) maintenir cette liste lors de suppression ou insertion de point
    // 3) générer aléatoire point à insérer ou à supprimer

    Domain() {}; //needed by rcpp_delaunay_module.cpp
    Domain(std::vector<double> left_,std::vector<double> right_) {//Rectangle first
        left=left_;right=right_;
        dim=left.size();
        //DEBUG: std::cout << "dim="<< dim << std::endl;
        set_size();
    };

    NumericVector pick() {
        std::vector<double> newPt(dim);
        for(int i=0;i<dim;i++) newPt[i]=as<double>(runif(1,left[i],right[i]));
        return NumericVector(newPt.begin(),newPt.end());
    };


    double get_size() {return size;} //area or volume

    int get_dim() {return dim;}


    std::vector<double> left,right; //in some axe
    std::vector<double> in_left,in_right; //in some axe

protected:

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