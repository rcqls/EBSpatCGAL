#ifndef RCPP_SIM_H
#define RCPP_SIM_H
#include <Rcpp.h>
#include "rcpp_term_expr.h"

using namespace Rcpp ;

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
            if(g < (domain->get_size())*exp(inter->single)) {
                inter->apply_INSERTION();
                (domain->inside_number)++;
            }
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

};

#endif //RCPP_SIM_H