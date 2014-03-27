#ifndef RCPP_TERM_EXPRESSION_H
#define RCPP_TERM_EXPRESSION_H

using namespace Rcpp ;

//Register interaction type here!!!
enum InterTypeID {
    //Point Process
    DEL1=0, DEL2, DEL3, DEL4, ALL1, ALL2
    //Tesselation here
};

enum TermMode {INSERTION=0,DELETION};

//To be called without knowing the class of children
class TermBase {
public:
    TermBase() {
    }

    virtual double eval_first_expr(void) = 0;

    virtual void set_point(NumericVector p) = 0;

    virtual void set_before_mode(int mode) = 0;

};

RCPP_EXPOSED_AS(TermBase)
RCPP_EXPOSED_WRAP(TermBase)

class Interaction {//equivalent of TermList
public:
    Interaction(List term_list_) {
        std::list<TermBase*> tl(term_list_.size());
        int i=0;
        for(
            List::iterator tit=term_list_.begin();
            tit != term_list_.end();
            ++tit,++i
        ) {
            tl.push_back(*tit);
        }
    }

    void set_point(NumericVector p) {
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit
        ) {
            (*lit)->set_point(p);
        }
    }

    double local_energy() {
        double res=single;

        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit
        ) {
            res += (*lit)->eval_first_expr();
        }

        return res;

    }
    //Single parameter
    double single;
private:

    std::list<TermBase*> term_list; //interaction term list

};

//STRUCT: class of the structure (ex: Delaunay2) , ELEMENT: current element (ex: Point_2), ELEMENT_HANDLE: (ex: Vertex_handle)
//ID: type of interaction, DIM: dimension, ORDER: structure order if needed (ex: ORDER=2 for Delaunay) 
//CONTAINER: class needed to update infos
template <InterTypeID ID, class STRUCT, class ELEMENT, class ELEMENT_HANDLE, class CONTAINER, int DIM=2, int ORDER=1>
class TermType : public TermBase { 

public:
    TermType() : TermBase() {
        Environment envir=Environment::global_env().new_child(true);
        mode_as_before=true; //Rmk: this is false only for simulation tricks
    }

    void set_struct(STRUCT* struct_) {structure=struct_;}

    STRUCT* get_struct() {return structure;}

    void set_exprs(List exprs_) { exprs=exprs_; }
    List get_exprs() { return exprs; }

    void set_cexprs(List cexprs_) { cexprs=cexprs_; }
    List get_cexprs() { return cexprs; }

    void set_infos(std::vector< std::string > infos_) { infos=infos_; }
    std::vector< std::string > get_infos() { return infos; }

    void set_params(List params_) {
    	params=params_;
    	import_vars(params);
    }

    List get_params() {return params;}

    void set_mode(int mode_) {mode=static_cast<TermMode>(mode_);}

    int get_mode() {return static_cast<int>(mode);}

    template <TermMode MODE>
    void set_current(NumericVector p); //by coordinates

    //just a wrapper of set_current to switch between mode operation
    void set_point(NumericVector p) {
        switch(mode) {
        case INSERTION: 
            set_current<INSERTION>(p);
            break;
        case DELETION: 
            set_current<DELETION>(p);
        }
    }

    NumericVector get_current();

    int get_current_index();

    double eval_first_expr() {
        double res=0;
        for(
            List::iterator lit=locBefore.begin();
            lit != locBefore.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            res -= as<double>(Rf_eval( exprs[0],envir));
        }

        for(
            List::iterator lit=locAfter.begin();
            lit != locAfter.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "after: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            res += as<double>(Rf_eval( exprs[0],envir));
        }

        return mode==INSERTION ? res : -res;
    }

    List eval_exprs() {

        std::list< std::vector<double> > res;
        int i,ii;
        for(i=0;i<exprs.size();i++) {
            std::vector<double> tmp(exprs_size[i],0);
            res.push_back(tmp);
        }
        for(
            List::iterator lit=locBefore.begin();
            lit != locBefore.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            i=0;
            for(
                std::list< std::vector<double> >::iterator rit=res.begin();
                rit != res.end();
                ++rit,++i
            ) {
                std::vector<double> tmp=as< std::vector<double> >(Rf_eval( exprs[i],envir));
                for(ii=0;ii<exprs_size[i];ii++) (*rit)[ii] -=  tmp[ii];
            }

        }

        for(
            List::iterator lit=locAfter.begin();
            lit != locAfter.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            i=0;
            for(
                std::list< std::vector<double> >::iterator rit=res.begin();
                rit != res.end();
                ++rit,++i
            ) {
                std::vector<double> tmp=as< std::vector<double> >(Rf_eval( exprs[i],envir));
                for(ii=0;ii<exprs_size[i];ii++) (*rit)[ii] += tmp[ii];
            }

        }

        //return mode==INSERTION ? res : -res;
        List ret=wrap(res);
        ret.attr("names") = exprs.attr("names");
        return ret;
    }

    // NumericVector eval_exprs() {
    //     NumericVector res(exprs.size());
    //     int i;
    
    //     for(
    //         List::iterator lit=locBefore.begin();
    //         lit != locBefore.end();
    //         ++lit
    //     ) {
    //         import_vars(*lit);
    //         //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
    //         for(i=0;i<exprs.size();i++)
    //             res[i] = res[i] - as<double>(Rf_eval( exprs[i],envir));
    //     }

    //     for(
    //         List::iterator lit=locAfter.begin();
    //         lit != locAfter.end();
    //         ++lit
    //     ) {
    //         import_vars(*lit);
    //         //DEBUG: std::cout << "after: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
    //         for(i=0;i<exprs.size();i++) 
    //             res[i] = res[i] + as<double>(Rf_eval( exprs[i],envir));
    //     }

    //     //return mode==INSERTION ? res : -res;
    //     return res;
    // }

    List update_infos(CONTAINER set);

    List update_infos(std::pair< CONTAINER,CONTAINER > sets);


    //used to
    bool mode_as_before;

    void set_before_mode(int mode_) {
        set_mode(mode_);
        mode_as_before=false;
    }

    template<TermMode MODE>
    void make_local_lists();

    List make_positive_list();

    List make_negative_list();

    void make_global_list();
 
    // list of local contributions (before and after) and global contributions
    List locBefore,locAfter,glob;

    IntegerVector exprs_size,cexprs_size;
private:
    //
    STRUCT* structure;
    //Term mode (ex: INSERTION for insertion) 
    TermMode mode;
    //Current element
    ELEMENT current;
    ELEMENT_HANDLE current_handle;//Handle of the current element
    int current_index; //Index of the current element
	//exprs: list of Language
	List exprs;	
	//cexprs : named list of Language
	// cexprs stand for common exprs which are repeated several times
	// cexprs is the information to save for each point 
	List cexprs;
	//infos: Vector of infos
	// an info is a predefined quantity depending on the TermEnergyType
	std::vector< std::string > infos;
	//params
	List params;
	//Environment
	Environment envir;

    //To import variables (inside List) to the environment
    void import_vars(List vars_) {
        CharacterVector names=vars_.names();
        int l=vars_.size();

        for(int i = 0; i < l ; i++) {
            SEXP name = Rf_install(Rf_translateChar(STRING_ELT(names, i)));
            Rf_defineVar(name, VECTOR_ELT(vars_, i), envir);
        }
    }

};



#endif //RCPP_TERM_EXPRESSION_H