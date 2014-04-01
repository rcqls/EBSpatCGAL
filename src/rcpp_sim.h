#ifndef RCPP_SIM_H
#define RCPP_SIM_H
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

    double get_size() {return size;}
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

//Have to know about the 
template <typename STRUCT>
class SimGibbs { 

	public:
        SimGibbs() {
         //domain=new Domain<STRUCT>()
        	//inter=new Interaction(inter_);
            //nb_pts=;
            nb_runs=10000;
        }

        SimGibbs(List inter_,Domain<STRUCT>* domain_ ) {
            set_interaction(inter_);
            set_domain(domain_);
            nb_runs=10000;
        }

        SimGibbs(List inter_,STRUCT* structure_,std::vector<double> left_,std::vector<double> right_) {
            set_interaction(inter_);
            set_domain(structure_,left_,right_);
            nb_runs=10000;
        }

        void set_interaction(List inter_) {
            inter=new Interaction(inter_);
        }

        void set_domain(Domain<STRUCT>* domain_ ) {
            domain=domain_;
        }

        void set_domain(STRUCT* structure_,std::vector<double> left_,std::vector<double> right_) {
            domain=new Domain<STRUCT>(structure_,left_,right_);
        }

        void set_single(double single_) {inter->single=single_;}

        double get_single() {return inter->single;}

        Domain<STRUCT>* get_domain() {return domain;}

        void run() {
            init();
            inter->sim_mode(true);
            for(int i=0;i<nb_runs;i++) run_once();
            inter->sim_mode(false);
        }

        int nb_runs;

        int nb_pts;

        double action_split;
    private:

        Domain<STRUCT>* domain;

        Interaction* inter; //For Sim only the first expression is supposed to contain the local energy formula

        void init() {
            action_split=0.5;
            RNGScope scope;
        }

        TermMode propose_action() {
            double r = as<double>(runif(1,0,1));
            TermMode action=r<=action_split ? INSERTION : DELETION;
            //DEBUG: std::cout << "action: "<<action << std::endl;
            return action;
        };

        double value_INSERTION() {
            //self->area/(DOUBLE)nb_dv_total*EXP(-(self->local_energy)(func,poly))
            return (domain->get_size())/(domain->inside_number)*exp(-(inter->local_energy()));
        } 

        double value_DELETION() {
            //Rmk: inter->local_energy returns insertion local energy only
            return (domain->inside_number)/(domain->get_size())*exp(-(inter->local_energy()));
        } 

        void run_once() {

            double g = as<double>(runif(1,0,1)),val;
            if( domain->inside_number == 0 ) {
                inter-> set_current(domain->propose_INSERTION());
                if(g < (domain->get_size())*exp(inter->single)) {}
            } else if(propose_action() == INSERTION) {  
                inter-> set_current(domain->propose_INSERTION());
                val=value_INSERTION();
                //DEBUG: std::cout << "nb=" << domain->inside_number << " g=" << g << " value_INSERTION=" << val << std::endl;
                if(g < val ) (domain->inside_number)++;
                else inter->apply_DELETION();
            } else {//else propose_action() == DELETION
                inter-> set_current(domain->propose_DELETION());
                val=value_DELETION();
                //DEBUG: std::cout << "nb=" << domain->inside_number << " g=" << g << " value_DELETION=" << val << std::endl;
                if(g <  val) (domain->inside_number)--;
                else inter->apply_INSERTION();
            }

        };


//   double g,g1,p,x,y,V_x_phi;
//   int n,nb_dv_total;
   
   
      
 

   
   
//     nb_dv_total=vor->nb_dv_total-self->nb_dv_totalExt;
//   //Rprintf("nb_dv_total(sim)=%d=(%d-%d)\n",nb_dv_total,vor->nb_dv_total,self->nb_dv_totalExt);
//   //nécessairement de l'insertion!!!!
//   if(nb_dv_total==0) {
 
//     pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(vor);
//     ebsim_gen_dv(self,pt_dv,self->size,self->center);
//     ebpoly_make_ins(poly,pt_dv);
//     GetRNGstate();
//     g=unif_rand();
//     PutRNGstate();
 
//     if(g < self->area*EXP(single)) {//QUASI INUTILE PUISQUE CELA DOIT TOUJOURS ETRE VRAI!!! -> ne plus le spécifier dans ebsim_func!!!
 
//       ebpoly_apply_ins(poly);
 
//     }
//     ebpoly_final_ins(poly);
   
//   } else {
//     // suppression d'un point de la configuration
//     //      le choix du point est aleatoire      
//     GetRNGstate();
//     g1=unif_rand();
//     PutRNGstate();
 
//     if(g1 < .5) {
 
//       GetRNGstate();
//       do {
//         n=(int)(unif_rand()*(vor->nb_dv_total));
 
//         pt_dv=ebvor_dv_at(vor,n);
 
//         //simulation dans tout le domaine ou point dans le domaine intérieur --> ok! 
//       } while((self->size[0] < vor->size[0]) && (self->size[1] < vor->size[1]) && !ebdv_dans_domaineIn(pt_dv,vor));
//       ebpoly_make_sup(poly,pt_dv);
//       g=unif_rand();
//       PutRNGstate();
 
 
//         if(g < (DOUBLE)nb_dv_total/self->area*EXP((self->local_energy)(func,poly))) {
  
//           ebpoly_apply_sup(poly);
 
//         }
//         ebpoly_final_sup(poly);
 
//     } else {
    
//     // insertion d'un point dans configuration 
//     // le point est choisi uniformement dans S 
//       pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(vor);
//       ebsim_gen_dv(self,pt_dv,self->size,self->center);
//       ebpoly_make_ins(poly,pt_dv);
//       //printf("Sortie: ebpoly_make_ins\n");
//       GetRNGstate();
//       g=unif_rand();
//       PutRNGstate();
 
       
//       if(g < self->area/(DOUBLE)nb_dv_total*EXP(-(self->local_energy)(func,poly))) {
  
//         ebpoly_apply_ins(poly);
 
//       }
//       ebpoly_final_ins(poly);
 
//     }
//   }
 
// }


};

#endif //RCPP_SIM_H