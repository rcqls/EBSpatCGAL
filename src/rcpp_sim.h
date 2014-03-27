#ifndef RCPP_SIM_H
#define RCPP_SIM_H
#include <Rcpp.h>

using namespace Rcpp ;

//Have to know about the 
template <class STRUCT, class ELEMENT, class ELEMENT_HANDLE>
class SimGibbs { 

	public:
        SimGibbs(List term_list_) {
        	term_list=term_list_;
        }

        void run() {
            for(int i=0;i<nb_runs;i++) run_once();
        }

        int nb_runs;
    private:
        List term_list; //For Sim only the first expression is supposed to contain the local energy formula

        void init() {

        }

        void run_once() {

        }

};

#endif //RCPP_SIM_H