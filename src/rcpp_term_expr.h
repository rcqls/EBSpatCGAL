#ifndef RCPP_TERM_EXPRESSION_H
#define RCPP_TERM_EXPRESSION_H
#include "rcpp_dom.h"

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

    virtual ~TermBase() {};
 
    virtual double eval_first_expr(void) = 0;

    virtual void set_current(NumericVector p) = 0;

    virtual int get_mode() = 0;

    virtual List get_mark() = 0;

    virtual void set_mark(List mark) = 0;

    virtual int inside_number(Domain* domain) = 0;

    virtual void apply_INSERTION() =0;

    virtual void complete_INSERTION(TermBase* term_)=0;

    virtual void apply_DELETION() =0;

    //boolean to know if make_local_lists is automatically generated when current is set.
    bool auto_make_list;

    // list of local contributions (before and after) and global contributions
    // Here after and before refer to insertion mode. Generally, it is related to current
    virtual List make_after_list()=0;
    virtual List make_before_list()=0;
    virtual List get_after_list()=0;
    virtual List get_before_list()=0;
    virtual List get_after_cexprs()=0;
    virtual List get_before_cexprs()=0;
    virtual void set_cexprs_caches(List cexprs_cache_)=0;
    virtual void put_exprs_from_cache_at(int i,std::string cache,Environment envir)=0;
    virtual List get_exprs_from_cache_at(int i,std::string cache)=0;
};

RCPP_EXPOSED_AS(TermBase);
RCPP_EXPOSED_WRAP(TermBase);


class TermList {//List of TermBase (heterogeneous terms) 
public:
    TermList(List term_list_) {
        make_local_lists_last_apply=true;

        for(
            List::iterator tit=term_list_.begin();
            tit != term_list_.end();
            ++tit
        ) {
            term_list.push_back(*tit);
        }
        first_term=term_list.front();
    }

    // delegators to first_term (avoid STRUCT templating in SimGibbs and ListCache)

    int inside_number(Domain* domain) {
        return first_term->inside_number(domain);
    }

    void apply_INSERTION() {
        first_term->apply_INSERTION();
    }

    void apply_DELETION() {
        first_term->apply_DELETION();
    }

    void set_mark(List mark) {
        first_term->set_mark(mark);
    }

    List get_mark() {return first_term->get_mark();}

    // the other methods

    void make_local_lists() {
        //DEBUG: std::cout << "mode=" << first_term->get_mode() << std::endl;
        if(first_term->get_mode()==INSERTION) {//INSERTION mode
            for(
                std::list<TermBase*>::iterator tit=term_list.begin();
                tit != term_list.end();
                ++tit
            ) {
                (*tit)->make_before_list();
            }

            //DEBUG: std::cout << "apply_INSERTION" << std::endl;
            apply_INSERTION();
            
            for(
                std::list<TermBase*>::iterator tit=term_list.begin();
                tit != term_list.end();
                ++tit
            ) {
                (*tit)->complete_INSERTION(first_term); //if point already inserted
                (*tit)->make_after_list();
            }
            //DEBUG: std::cout << "apply_DELETION" << std::endl;
            if(make_local_lists_last_apply) apply_DELETION();

        } else {//DELETION mode
            for(
                std::list<TermBase*>::iterator tit=term_list.begin();
                tit != term_list.end();
                ++tit
            ) {
                (*tit)->make_after_list();
            }

            apply_DELETION();
            
            for(
                std::list<TermBase*>::iterator tit=term_list.begin();
                tit != term_list.end();
                ++tit
            ) {
                (*tit)->make_before_list();
            }
            if(make_local_lists_last_apply) apply_INSERTION();
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

    bool make_local_lists_last_apply;
};


class Interaction: public TermList {
public:

    Interaction(List term_list_): TermList(term_list_) {
        envir=Environment::global_env().new_child(true);
    }

    List new_mark() {
        return Rf_eval( mark_expr,R_GlobalEnv );
    }

    void set_mark_expr(Language expr) {
        mark_expr = expr;
    }

    void set_current_mark() {TermList::set_mark(new_mark());}

    void sim_mode(bool mode) {
        make_local_lists_last_apply = !mode;
    }
    
    double local_energy() {
        double res=single;

        // prepare local lists before evaluating first expr
        make_local_lists();

        int i=0;
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit,++i
        ) {
            //DEBUG: std::cout << "res["<< i << "]=" << res << std::endl;
            //DEBUG: double res2=(*lit)->eval_first_expr();
            //DEBUG: std::cout << "res2["<< i << "]=" << res2 << std::endl;
            res += (*lit)->eval_first_expr();
        }

        return res;

    }

    //Save infos only!
    List get_local_lists() {

        make_local_lists_last_apply = true;

        List res(term_list.size()); 

        // prepare local lists before evaluating first expr
        make_local_lists();

        int i=0;
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit,++i
        ) {
            //DEBUG: std::cout << "res["<< i << "]=" << res << std::endl;
            //DEBUG: double res2=(*lit)->eval_first_expr();
            //DEBUG: std::cout << "res2["<< i << "]=" << res2 << std::endl;
            List ret;
            ret["before"] = (*lit)->get_before_list();
            ret["after"] = (*lit)->get_after_list();
            res[i]=ret;
        }

        return res;

    }

    List get_cexprs() {

        make_local_lists_last_apply = true;

        List res(term_list.size()); 

        // prepare local lists before evaluating first expr
        make_local_lists();

        int i=0;
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit,++i
        ) {
            //DEBUG: 
            //std::cout << "ici" << std::endl;
            //DEBUG: double res2=(*lit)->eval_first_expr();
            //DEBUG: std::cout << "res2["<< i << "]=" << res2 << std::endl;
            List ret;
            ret["before"] = (*lit)->get_before_cexprs();
            ret["after"] = (*lit)->get_after_cexprs();
            res[i]=ret;
        }

        return res;

    }

    void set_cexprs_caches(List cexprs_cache) {

        int i=0;
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit,++i
        ) {
            List first=cexprs_cache["first"],second=cexprs_cache["second"];
            (*lit)->set_cexprs_caches(List::create(_["first"]=first[i],_["second"]=second[i]));
        }        

    }

    
    //exprs for caches
    List first_cache_exprs,second_cache_exprs;
    //and sizes of caches
    int first_cache_size,second_cache_size;
    //and envir for computing exprs from caches
    Environment envir;

    void set_exprs_for_caches(List caches_exprs) {
        first_cache_exprs=caches_exprs["first"];
        second_cache_exprs=caches_exprs["second"];
    }

    void set_sizes_for_caches(IntegerVector caches_sizes) {
        first_cache_size=caches_sizes[0];
        second_cache_size=caches_sizes[1];
        std::cout << "first size=" << first_cache_size << std::endl;
        std::cout << "second size=" << second_cache_size << std::endl;
    }

    void set_single(NumericVector single) {
        Rf_defineVar(Rf_install("single"), single, envir);
    }

    void get_first_terms_exprs_at(int i) {
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit
        ) {
            (*lit)->put_exprs_from_cache_at(i,"first",envir);
        }
    }

    void get_second_terms_exprs_at(int i) {
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit
        ) {
            (*lit)->put_exprs_from_cache_at(i,"second",envir);
        }
    }

    //get exprs .f as a list from cexprs caches
    List get_exprs_from_cexprs_caches() {
        List resFirst(first_cache_size),resSecond(second_cache_size);
        int i;
        for(i=0;i<first_cache_size;i++) {
            List res;
            for(
                std::list<TermBase*>::iterator lit=term_list.begin();
                lit != term_list.end();
                ++lit
            ) {
                List ret=(*lit)->get_exprs_from_cache_at(i,"first");
                std::vector<std::string> names=ret.names();
                for(std::vector<std::string>::iterator nit=names.begin();nit!=names.end();++nit) {
                    res[*nit]=ret[*nit];
                }
            }
            resFirst[i]=res;
        }

        for(i=0;i<second_cache_size;i++) {
            List res;
            for(
                std::list<TermBase*>::iterator lit=term_list.begin();
                lit != term_list.end();
                ++lit
            ) {
                List ret=(*lit)->get_exprs_from_cache_at(i,"second");
                std::vector<std::string> names=ret.names();
                for(std::vector<std::string>::iterator nit=names.begin();nit!=names.end();++nit) {
                    res[*nit]=ret[*nit];
                }
            }
            resSecond[i]=res;
        }

        return List::create(_["first"]=resFirst,_["second"]=resSecond);
        
    } 

    //eval first exprs for indexes

    NumericVector eval_first_exprs_from_cexprs_caches(std::vector<int> indexes) {
        std::vector<double> resFirst(indexes.size());

        for(int i=0;i<first_cache_size;i++) {
            get_first_terms_exprs_at(i); //put .f exprs inside envir
            for(std::vector<int>::iterator it=indexes.begin();it!=indexes.end();++it)
                resFirst[*it] += as<double>(Rf_eval(first_cache_exprs[*it],envir)); //eval jth expr form .f exprs
        }

        NumericVector firstResult(resFirst.begin(),resFirst.end());

        return firstResult;
        
    }

    //eval second exprs for indexes

    NumericVector eval_second_exprs_from_cexprs_caches(std::vector<int> indexes) {
        std::vector<double> resSecond(indexes.size());

        for(int i=0;i<second_cache_size;i++) {
            get_second_terms_exprs_at(i); //put .f exprs inside envir
            for(std::vector<int>::iterator it=indexes.begin();it!=indexes.end();++it)
                resSecond[*it] += as<double>(Rf_eval(second_cache_exprs[*it],envir)); //eval jth expr form .f exprs
        }

        NumericVector secondResult(resSecond.begin(),resSecond.end());

        return secondResult;
    }

    //eval all exprs

    List eval_exprs_from_cexprs_caches() {
        std::vector<double> resFirst(first_cache_exprs.size()),resSecond(second_cache_exprs.size());
        int i,j;
        for(i=0;i<first_cache_size;i++) {
            get_first_terms_exprs_at(i); //put .f exprs inside envir
            for(j=0;j<first_cache_exprs.size();j++)
                resFirst[j] += as<double>(Rf_eval(first_cache_exprs[j],envir)); //eval jth expr form .f exprs
        }

        for(i=0;i<second_cache_size;i++) {
            get_second_terms_exprs_at(i); //put .f exprs inside envir
            for(j=0;j<second_cache_exprs.size();j++)
                resSecond[j] += as<double>(Rf_eval(second_cache_exprs[j],envir)); //eval jth expr form .f exprs
        }

        NumericVector firstResult(resFirst.begin(),resFirst.end()),secondResult(resSecond.begin(),resSecond.end());
        return List::create(_["first"]=firstResult,_["second"]=secondResult);
    }
/*************************************************************/
/* I made a mistake! This is not what I want mathematically! */
/*************************************************************
    void set_exprs_cexprs_caches(List cexprs_cache) {

        int i=0;
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit,++i
        ) {
            List first=cexprs_cache["first"],second=cexprs_cache["second"];
            (*lit)->set_cexprs_caches(List::create(_["first"]=first[i],_["second"]=second[i]));
        }        

    }

    List eval_exprs_from_cexprs_caches() {
        //set param is supposed  to be applied just before
        List resFirst,resSecond;
        int i=0;
        for(
            std::list<TermBase*>::iterator lit=term_list.begin();
            lit != term_list.end();
            ++lit,++i
        ) {
            //std::cout << "i: " << i << std::endl;
            List resCaches=(*lit)->eval_exprs_from_cexprs_caches();
            List resFirstCache=resCaches["first"],resSecondCache=resCaches["second"];
            //std::cout << "i: " << i << std::endl;
            std::vector<std::string> varnames=resFirstCache.names();
            //std::cout << "i: " << i << std::endl;
            for(
                std::vector<std::string>::iterator vit=varnames.begin();
                vit != varnames.end();
                ++vit
            ) {
                //std::cout << "var: " << *vit << std::endl; 
                resFirst[*vit]=resFirstCache[*vit];
                resSecond[*vit]=resSecondCache[*vit];
            }
        }        

        return List::create(_["first"]=resFirst,_["second"]=resSecond);

    } 
    *********************************************************/

    //Weird: but I guess it is because does not know how to manage inheritance yet!
    //TODO: to delete when Rcpp would fix that
    void set_current(NumericVector p) {TermList::set_current(p);}

    // set_mark is defined in TermList!

    //Single parameter
    double single;

private:
    Language mark_expr;

};

//RMK: No INTEREST to embed Lines and Points in the same template!
//     Just try to adapt the same idea if desired!

//STRUCT: class of the structure (ex: Delaunay2) , ELEMENT: current element (ex: Point_2), HANDLE: (ex: Vertex_handle)
//ID: type of interaction, DIM: dimension, ORDER: structure order if needed (ex: ORDER=2 for Delaunay)  
template <InterTypeID ID, typename STRUCT, typename ELEMENT = typename STRUCT::Point, typename HANDLE = typename STRUCT::Vertex_handle, int ORDER=1 >
class TermType : public TermBase { 

public:
    typedef HANDLE Handle;
    typedef std::set<HANDLE> HandleSet;
    typedef std::set<HandleSet> HandleSet_Set;

    TermType() : TermBase() {
        envir=Environment::global_env().new_child(true);
    }

    Environment get_envir() {return envir;}

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
    void set_current(NumericVector p) {
        mode= p.size()==1 ? DELETION : INSERTION;
        switch(mode) {
        case INSERTION: 
            set_current_<INSERTION>(p);
            break;
        case DELETION: 
            set_current_<DELETION>(p);
        }
    }

    NumericVector get_current();

    int get_current_index();

    //only needed for the first_term of Interaction object
    void set_mark(List mark) {
        current_info=mark;
    }

    List get_mark() {return current_handle->info();}

    int inside_number(Domain* domain) {return structure->number_of_vertices();};

    //Maybe to secialize if needed (when trying to embed Kiên stuff)!
    //An alternative is to adapt this part!
    

    //HERE I need to comment because insert actually returns vertex_handle and graph remains unchanged
    // when the point already exists.
    void apply_INSERTION() {
        current_handle=structure->insert(current);
        current_handle->info()=current_info;
    }


    // MAYBE to replace with apply_INSERTION() in TermList class!!!!
    // The following could be replaced with apply_INSERTION()!!!
    void complete_INSERTION(TermBase* term_) {
        if(this == term_) return;
        current_handle=static_cast<TermType*>(term_)->current_handle;
    };

    void apply_DELETION() {
        //save current_info first
        current_info=current_handle->info();
        structure->remove(current_handle);
    }

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
        //DEBUG: std::cout << "after before: " << res << std::endl;
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
        //DEBUG: std::cout << "after res: " << res << std::endl;
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
        int i;
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

    List update_infos(std::vector<Handle> set);

    List update_infos(HandleSet_Set set);

    List update_infos(std::pair< HandleSet_Set,HandleSet_Set > sets);


    template<TermMode MODE>
    void make_local_lists();

    void make_global_list();

    List make_after_list();
    List make_before_list();

    List get_after_list() {return locAfter;};
    List get_before_list() {return locBefore;};

    // compute List of values from a list of cexprs and vars List 
    List eval_cexprs(List infos_list) {
        List ret(infos_list.size());
        int i=0;
        for(
            List::iterator it=infos_list.begin();
            it != infos_list.end();
            ++it,++i

        ) {
            List res(cexprs.size());
            int ii=0;
            import_vars(*it);
            for(
                List::iterator lit=cexprs.begin();
                lit != cexprs.end();
                ++lit,++ii
            ) {
                res[ii]=Rf_eval(cexprs[ii],envir);
            }
            res.names() = cexprs.names();

            ret[i]=res;
        }
        
        return ret;

    }

    List get_after_cexprs() {return eval_cexprs(locAfter);};
    List get_before_cexprs() {return eval_cexprs(locBefore);};

    void set_cexprs_caches(List cexprs_caches_) {
        cexprs_caches=cexprs_caches_;
    }

    List get_cexprs_caches() {
        return cexprs_caches;
    }

    void put_exprs_from_cache_at(int i,std::string cacheName,Environment envir) {
        List cache=as<List>(cexprs_caches[cacheName])[i];
        //std::cout << current_cache << std::endl;
        init_exprs_results();
        //std::cout << current_cache << std::endl;
        eval_before_exprs_results_from_cexprs_cache(cache["before"]);
        //std::cout << current_cache << std::endl;
        eval_after_exprs_results_from_cexprs_cache(cache["after"]);
        //std::cout << current_cache << std::endl;
        exprs_results_to_envir(envir);

    }

    List get_exprs_from_cache_at(int i,std::string cacheName) {
        List cache=as<List>(cexprs_caches[cacheName])[i];
        //std::cout << current_cache << std::endl;
        init_exprs_results();
        //std::cout << current_cache << std::endl;
        eval_before_exprs_results_from_cexprs_cache(cache["before"]);
        //std::cout << current_cache << std::endl;
        eval_after_exprs_results_from_cexprs_cache(cache["after"]);
        //std::cout << current_cache << std::endl;
        return exprs_results_as_List();

    }

    void exprs_results_to_envir(Environment envir_) {

        List res=wrap(exprs_results);
        CharacterVector names=exprs.names();

        for(int i = 0; i < names.size() ; i++) {
            //std::cout << "var:" << names[i] << std::endl;
            SEXP name = Rf_install(Rf_translateChar(STRING_ELT(names, i)));
            Rf_defineVar(name, VECTOR_ELT(res, i), envir_);
            //Rf_defineVar(name,  wrap(exprs_results(i), envir);
        }
    }


    void eval_before_exprs_results_from_cexprs_cache(List beforeCache) {
        int i,ii;
        for(
            List::iterator lit=beforeCache.begin();
            lit != beforeCache.end();
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

    void eval_after_exprs_results_from_cexprs_cache(List afterCache) {
        int i,ii;
        for(
            List::iterator lit=afterCache.begin();
            lit != afterCache.end();
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

    /******************************************* The same mistake!
    List eval_exprs_from_cexprs_caches() {
        //First
        current_cache="first";
        //std::cout << current_cache << std::endl;
        init_exprs_results();
        //std::cout << current_cache << std::endl;
        eval_before_exprs_results_from_cexprs_caches();
        //std::cout << current_cache << std::endl;
        eval_after_exprs_results_from_cexprs_caches();
        //std::cout << current_cache << std::endl;
        List firstResult=exprs_results_as_List();
        //Second
        current_cache="second";
        //std::cout << current_cache << std::endl;
        init_exprs_results();
        eval_before_exprs_results_from_cexprs_caches();
        eval_after_exprs_results_from_cexprs_caches();
        List secondResult=exprs_results_as_List();
        return List::create(_["first"]=firstResult,_["second"]=secondResult);
    } 
    ***************************************************/

    List locBefore,locAfter,glob;

    IntegerVector exprs_size,cexprs_size;
private:
    //
    STRUCT* structure;
    //Term mode (ex: INSERTION for insertion) 
    TermMode mode;
    //Current element
    ELEMENT current;
    HANDLE current_handle;//Handle of the current element
    List current_info;
    int current_index; //Index of the current element

	//exprs: list of Language
	List exprs;
    //cexprs: List of Language
    List cexprs;
    List cexprs_caches;
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