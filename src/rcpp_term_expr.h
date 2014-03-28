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
        //Modifier when creating local lists
        auto_make_list=true; //Rmk, this has to be false for Interaction
    }

    virtual double eval_first_expr(void) = 0;

    virtual void set_current(NumericVector p) = 0;

    virtual int get_mode() = 0;

    virtual void apply_insert() =0;

    virtual void apply_delete() =0;

    //boolean to know if make_local_lists is automatically genertaed when current is set.
    bool auto_make_list;

    // list of local contributions (before and after) and global contributions
    // Here after and before refer to insertion mode. Generally, it is related to current
    virtual List make_after_list()=0;
    virtual List make_before_list()=0;

};

RCPP_EXPOSED_AS(TermBase);
RCPP_EXPOSED_WRAP(TermBase);

class TermList {//List of TermBase (heterogeneous terms) 
public:
    TermList(List term_list_) {
        for(
            List::iterator tit=term_list_.begin();
            tit != term_list_.end();
            ++tit
        ) {
            term_list.push_back(*tit);
        }
        first_term=term_list.front();
    }

    void make_local_lists() {
        if(first_term->get_mode()==INSERTION) {
            for(
                std::list<TermBase*>::iterator tit=term_list.begin();
                tit != term_list.end();
                ++tit
            ) {
                (*tit)->make_before_list();
            }
            first_term->apply_insert();
            for(
                std::list<TermBase*>::iterator tit=term_list.begin();
                tit != term_list.end();
                ++tit
            ) {
                (*tit)->make_after_list();
            }
            first_term->apply_delete();

        }

    }

    void set_current(NumericVector p) {
        for(
            std::list<TermBase*>::iterator tit=term_list.begin();
            tit != term_list.end();
            ++tit
        ) {
            (*tit)->auto_make_list=false; //no more lists made in set_current 
            (*tit)->set_current(p);
        }
    }

protected:

    std::list<TermBase*> term_list; //interaction term list
    TermBase* first_term;


};

class Interaction: public TermList {
public:
    Interaction(List term_list_): TermList(term_list_) {}
    
    double local_energy() {
        double res=single;

        make_local_lists();

        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit
        ) {
            //double res2=(*lit)->eval_first_expr();
            //std::cout << "res2=" << res2 << std::endl;
            res += (*lit)->eval_first_expr();
        }

        return res;

    }

    //Weird: but I guess it is because does not know how to manage inheritance yet!
    //TODO: to delete when Rcpp would fix that
    void set_current(NumericVector p) {TermList::set_current(p);}


    //Single parameter
    double single;
};

//STRUCT: class of the structure (ex: Delaunay2) , ELEMENT: current element (ex: Point_2), ELEMENT_HANDLE: (ex: Vertex_handle)
//ID: type of interaction, DIM: dimension, ORDER: structure order if needed (ex: ORDER=2 for Delaunay) 
//CONTAINER: class needed to update infos
template <InterTypeID ID, class STRUCT, class ELEMENT, class ELEMENT_HANDLE, class CONTAINER, int DIM=2, int ORDER=1>
class TermType : public TermBase { 

public:
    TermType() : TermBase() {
        envir=Environment::global_env().new_child(true);
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
    void set_current_(NumericVector p); //by coordinates

    //just a wrapper of set_current_ depending on mode
    void set_current(NumericVector p);

    NumericVector get_current();

    int get_current_index();

    //Maybe to secialize if needed (when trying to embed Kiên stuff)!
    void apply_insert() {current_handle=structure->insert(current);}
    void apply_delete() {structure->remove(current_handle);}

    double eval_before_first_expr() {
        double res=0;
        for(
            List::iterator lit=locBefore.begin();
            lit != locBefore.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            res += as<double>(Rf_eval( exprs[0],envir));
        }

        return res;
    };

    double eval_after_first_expr() {
        double res=0;

        for(
            List::iterator lit=locAfter.begin();
            lit != locAfter.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "after: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            res += as<double>(Rf_eval( exprs[0],envir));
        }

        return res;
    }

    // As in INSERTION mode!
    double eval_first_expr() {
        //return mode==INSERTION ? eval_after_first_expr() - eval_before_first_expr() : eval_before_first_expr() - eval_after_first_expr();
        return eval_after_first_expr() - eval_before_first_expr();
    }

    std::list< std::vector<double> > exprs_results;

    // (init|eval_before|eval|after)_exprs_results methods allow us to split the evaluation into several functions
    // mainly used inside eval_exprs method below
    void init_exprs_results() {
        int i,ii;
        exprs_results.clear();
        for(i=0;i<exprs.size();i++) {
            std::vector<double> tmp(exprs_size[i],0);
            exprs_results.push_back(tmp);
        }
    }

    void eval_before_exprs_results() { 
        int i,ii;
        for(
            List::iterator lit=locBefore.begin();
            lit != locBefore.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            i=0;
            for(
                std::list< std::vector<double> >::iterator rit=exprs_results.begin();
                rit != exprs_results.end();
                ++rit,++i
            ) {
                std::vector<double> tmp=as< std::vector<double> >(Rf_eval( exprs[i],envir));
                for(ii=0;ii<exprs_size[i];ii++) (*rit)[ii] -=  tmp[ii];
            }

        }

    };

    void eval_after_exprs_results() {
        int i,ii;
        for(
            List::iterator lit=locAfter.begin();
            lit != locAfter.end();
            ++lit
        ) {
            import_vars(*lit);
            //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
            i=0;
            for(
                std::list< std::vector<double> >::iterator rit=exprs_results.begin();
                rit != exprs_results.end();
                ++rit,++i
            ) {
                std::vector<double> tmp=as< std::vector<double> >(Rf_eval( exprs[i],envir));
                for(ii=0;ii<exprs_size[i];ii++) (*rit)[ii] += tmp[ii];
            }

        }
        
    };

    List exprs_results_as_List() {
        List ret=wrap(exprs_results);
        ret.attr("names") = exprs.attr("names");
        return ret;
    };

    // Standalone eval exprs method
    List eval_exprs() {

        init_exprs_results();
        eval_before_exprs_results();
        eval_after_exprs_results();
        return exprs_results_as_List();

    };

    /****************** OBSOLETE ***************/
    // List eval_exprs() {

    //     std::list< std::vector<double> > res;
    //     int i,ii;
    //     for(i=0;i<exprs.size();i++) {
    //         std::vector<double> tmp(exprs_size[i],0);
    //         res.push_back(tmp);
    //     }
    //     for(
    //         List::iterator lit=locBefore.begin();
    //         lit != locBefore.end();
    //         ++lit
    //     ) {
    //         import_vars(*lit);
    //         //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
    //         i=0;
    //         for(
    //             std::list< std::vector<double> >::iterator rit=res.begin();
    //             rit != res.end();
    //             ++rit,++i
    //         ) {
    //             std::vector<double> tmp=as< std::vector<double> >(Rf_eval( exprs[i],envir));
    //             for(ii=0;ii<exprs_size[i];ii++) (*rit)[ii] -=  tmp[ii];
    //         }

    //     }

    //     for(
    //         List::iterator lit=locAfter.begin();
    //         lit != locAfter.end();
    //         ++lit
    //     ) {
    //         import_vars(*lit);
    //         //DEBUG: std::cout << "before: " << as<double>(Rf_eval( exprs[0],envir)) << std::endl;
    //         i=0;
    //         for(
    //             std::list< std::vector<double> >::iterator rit=res.begin();
    //             rit != res.end();
    //             ++rit,++i
    //         ) {
    //             std::vector<double> tmp=as< std::vector<double> >(Rf_eval( exprs[i],envir));
    //             for(ii=0;ii<exprs_size[i];ii++) (*rit)[ii] += tmp[ii];
    //         }

    //     }

    //     //return mode==INSERTION ? res : -res;
    //     List ret=wrap(res);
    //     ret.attr("names") = exprs.attr("names");
    //     return ret;
    // };

    /****************** OBSOLETE ***************/
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
    /****************** OBSOLETE ***************/

    List update_infos(CONTAINER set);

    List update_infos(std::pair< CONTAINER,CONTAINER > sets);


    template<TermMode MODE>
    void make_local_lists();

    void make_global_list();

    List make_after_list();
    List make_before_list();

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