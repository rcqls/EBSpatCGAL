# Example:
# lc <- GNZCache(del2 ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => params(lc,th=c(2,4)) and update(lc,current=del2)



## ... or forms

GNZCache <-function(model,...,runs=1000,domain=c(-350,-350,350,350),forms) {
	formMngr <- ComponentFunctionalFormulaManager()
	formula(formMngr,model,local=TRUE)
	if(missing(forms)) forms <- as.list(substitute(c(...)))[-1]
	for(f in forms) formula(formMngr,f,local=TRUE) # First consider only the main case 
	#print(formula(formMngr))
	self <- newEnv(GNZCache,ParameterMngr,forms=forms,formMngr=formMngr,interMngr=InteractionMngr(formMngr$func,check.params=FALSE),runs=runs,domain=domain,mode=1) #mode=0 or 1
	# the first formula is supposed to model formula with response in the left part of the formula
	self$response <- if(formMngr$formulas[[1]][[1]]==as.name("~") && length(formMngr$formulas[[1]])==3) formMngr$formulas[[1]][[2]] else NULL
	if(!is.null(self$response)) {
		current <- try(eval.parent(self$response))
		if(inherits(current,"try-error")) {
			warning("No proper response in GNZCache!")
			self$struct <- NULL
		} else update(self,current)
	} else self$dim <- 2 # default value if no answer updatable if struct changed

	self$domain.volume <- if(self$dim==2) (self$domain[3]-self$domain[1])*(self$domain[4]-self$domain[2])
							else (self$domain[4]-self$domain[1])*(self$domain[5]-self$domain[2])*(self$domain[6]-self$domain[3])
			

	RcppPersistentObject(self,new = { 
		if(is.null(self$struct)) {
			## TODO: initialilize GNZCache without del2
			## Maybe create one! 
		} else {	
			rcpp <- new(GNZCacheCpp,terms(self$interMngr),self$domain[1:self$dim],self$domain[self$dim+(1:self$dim)])
			rcpp$single <- self$interMngr$single
			rcpp$nb_runs <- self$runs
			rcpp$set_sizes_for_interaction(c(as.integer(self$runs),as.integer(length(self$struct))))
			
			if(!is.null(self$interMngr$mark.name)) {
				rcpp$marked(TRUE)
				rcpp$mark_expr(self$interMngr$mark.expr)
				tmp <- as.list(rep(NA,length(self$interMngr$mark.name)))
				names(tmp) <- self$interMngr$mark.name
				self$struct$rcpp()$update_infinite_vertex_info(tmp)
			} else rcpp$marked(FALSE)
			# important for renew process!
			if(!is.null(self$struct)) update(self$interMngr,self$struct)
			self$to_make_lists <- TRUE
			rcpp
		} 
	})
	formula(self,"default")  #or exp(-(.V))*.form ~ .form
	self
}

# params, single and single<- dealt with ParameterMngr class

##########################################################################
# RMK: Interaction C++ object knows about STRUCT class via its first term 
# so no need to communicate the graph structure to GNZCache.
# This method is in charge to communicate the struct to all the terms of 
# the interaction manager.
##########################################################################
update.GNZCache <- function(self,current) {
	self$struct <- current
	# force renew of TermType taking into account of the new dimension of struct if necessary
	update(self$interMngr,self$struct)
	# change of dim with renew if necessary
	if(is.null(self$dim) || self$dim != self$struct$dim) {
		force <- !is.null(self$dim)
		self$dim <- self$struct$dim
		if(force) self$rcpp(TRUE) #force renew since change of dimension
	}
}


## RMK: If you have a single configuration of points you need to complete 
## this data with an implicit Simulable object! => TODO!!!
run.GNZCache <- function(self,current,...,runs,mode,domain,form) {
	params(self,...)

	rcpp <- self$rcpp()

	if(!missing(domain) && !identical(domain,self$domain)) {
		self$domain <- domain
		self$domain.volume <- if(self$dim==2) (self$domain[3]-self$domain[1])*(self$domain[4]-self$domain[2])
							else (self$domain[4]-self$domain[1])*(self$domain[5]-self$domain[2])*(self$domain[6]-self$domain[3])
	
		rcpp$set_domain(domain)
		self$to_make_lists <- TRUE
	}
	
	if(!missing(runs) && self$runs != runs) {
		self$runs <- runs
		rcpp$nb_runs <- self$runs
		rcpp$set_sizes_for_interaction(c(as.integer(self$runs),as.integer(length(seq(self$struct)))))
		# make_lists needed if change of runs
		self$to_make_lists <- TRUE
	}

	if(!missing(mode) && self$mode != mode) {
		self$mode <- mode
		rcpp$set_mode(mode)
		# make_lists needed if change of mode
		self$to_make_lists <- TRUE
	}
	

	if(!missing(current) && (is.null(self$struct) || identical(self$struct,current))) {
		if(inherits(current,"Simulable")) {
			update(self,current)
			self$to_make_lists <- TRUE
		} else cat("WARNING: object not of class Simulable!\n")
	}

	# to update caches when struct has been udated too
	if(!is.null(self$struct.uid) && self$struct$uid != self$struct.uid) {
		self$struct.uid <- self$struct$uid
		self$to_make_lists <- TRUE
	}

	if(self$to_make_lists) {
		cat("Please be patient: update of caches -> ")
		self$rcpp()$make_lists()
		self$to_make_lists <- FALSE
		cat("done! \n")
	}

	if(!missing(form)) {
		exprs.save <- self$exprs
		formula(self,form) #self$exprs updated
	} else exprs.save <- NULL

	if(!is.null(self$struct)) {
		tmp <- rcpp$eval_exprs()
		res <- list(first=tmp$first/self$runs,second=tmp$second/self$domain.volume)
		if(!identical(self$exprs,exprs.save)) {# restore self
			rcpp$set_exprs_for_interaction(exprs.save)
		}
		return(res)
	}
}

##################################################
# Pretty flexible!!
# 0) formula(self) show the formula
# 1) apply all the forms a the same template with .V representing the energy term and .form the functional term
# formula(self, exp(-(.V))*.form ~ .form) or formula(self,"default") or formula(self,"resid")
# formula(self,.form ~ exp(.V)*.form) or formula(self,"inverse")
# 2) another choice is to mix the different functionals (.form? with ? representing the rank of the functional to be used) 
# formula(self,expression(first=exp(-(.V))*.form1,second=.form1,first=.form1,second=exp(.V)*.form1))
# or formula(self,list(first="exp(-(.V))*.form1",second=".form1",first=".form1",second="exp(.V)*.form1"))
##################################################
formula.GNZCache <- function(self,form=NULL,add=FALSE) {

	if(!is.null(form)) { 
		#  reinit unless add=TRUE
		if(!add) self$exprs <- list(first=NULL,second=NULL)

		# form is character (and then transformed into formula object)
		if(is.character(form) && form %in% c("default","resid","inverse")) {
			form <- switch(form,
				inverse=.form ~ exp(.V) * .form,
				exp(-(.V))*.form ~ .form)
		}

		# form is a formula
		if(inherits(form,"formula")) {# formula mode!
			if(length(form)==2) stop("sorry: left and right terms are required")
			expr1 <- deparse(form[[2]])
			expr2 <- deparse(form[[3]])
			exprs<-list(
				first=sapply(self$formMngr$formulas[-1],
					function(form) {
						expr <- gsub("\\.V",paste("single+",deparse(self$formMngr$formulas[[1]][[3]]),sep=""),expr1)
						expr <- gsub("\\.form",deparse(form),expr)
						parse(text=expr)
					}),
				second=sapply(self$formMngr$formulas[-1],
					function(form) {
						expr <- gsub("\\.V",paste("single + ",deparse(self$formMngr$formulas[[1]][[3]]),sep=""),expr2)
						expr <- gsub("\\.form",deparse(form),expr)
						parse(text=expr)
					})
			)
			self$exprs$first  <- c(self$exprs$first,exprs$first)
			self$exprs$second <- c(self$exprs$second,exprs$second)
		} else {
			# form is a named expression or a named list of character
			
			# function to convert a list of single character formula to call
			convert <- function(form) {
				expr <- gsub("\\.V",paste("single + ",deparse(self$formMngr$formulas[[1]][[3]]),sep=""),form)
				gregexpr("\\.form([[:digit:]]*)",expr) -> tmp
				for(i in rev(1:length(tmp[[1]]))) {
					if(tmp[[1]][i]>0) {
						tmp2 <- attr(tmp[[1]],"match.length")[i]
						name <- substring(expr,tmp[[1]][i],tmp[[1]][i]+tmp2-1)
						ii <- as.integer(substring(name,6)) + 1
						if(!is.na(ii)) {
							expr <- paste(substring(expr,1,tmp[[1]][i]-1),
							deparse(self$formMngr$formulas[[ii]]),
							substring(expr,tmp[[1]][i]+tmp2),sep="")
						}
					}
				}
				#cat("expr>");print(expr)
				parse(text=expr)
			}

			# a named list of single character
			if(is.list(form) && length(form)==1 && names(form) %in% c("first","second")) {
				expr <- convert(form)
				if(names(form)=="first") self$exprs$first <- c(self$exprs$first,expr)
				else self$exprs$second <- c(self$exprs$second,expr)
			} else # a named list of character
			if(is.list(form) && length(form)>1 && all(names(form) %in% c("first","second"))) {
				for(i in seq(form)) formula(self,form[i],add=ifelse(i==1,add,TRUE))
			} else # a named expression
			if(is.expression(form) && all(names(form) %in% c("first","second"))) {
				formula(self,lapply(form,deparse),add=add)
			}

		}

		# inform the Interaction object
		self$rcpp()$set_exprs_for_interaction(self$exprs)
	}

	# echo the exprs
	self$exprs
}

##########################################################
# These plugins are only reusable code
# Used in pseudo.R (exponential mode) and tkinv.R
##########################################################
update_statex.GNZCachePlugin <- function(self) {
	# test if uid of struct changed
	if(is.null(self$struct.uid) || self$struct$uid != self$struct.uid) {
		self$struct.uid <- self$struct$uid
		self$to_make_lists <- TRUE
	}
	
	# test if cacheLists need to be updated and do it if so
	if(self$to_make_lists) {
		cat("Please be patient: update of caches -> ")
		self$rcpp()$make_lists()
		
		self$optim.update() #update self$optim.statex

		self$to_make_lists <- FALSE
		cat("done! \n")
	}
}
##########################################################
require_param_vect2list.GNZCachePlugin <- function(self,...) {
	if(is.null(self$param.vect2list)) {
		if(length(list(...))==0) stop("Need to initialize parameters values first!")
		params(self,...)
		self$param.vect2list<- Vector2ListConverter(sapply(params(self),function(e) sapply(e,length)))
	}
}

##########################################################
update_par0.GNZCachePlugin <- function(self,...) {
	if(length(list(...))==0) {
		do.call("params",c(list(self),by(self$param.vect2list,self$contrast$par)))
		self$contrast$par
	} else {
		if(!is.null(self$param.vect2list)) {
			params(self,...)
		}
		unlist(params(self))
	}
}
##########################################################

### NOT VERY USEFUL NOW since everything is done in C++
### Maybe, can help for debugging
# get.GNZCache <- function(self,mode=1,runs,transform="nothing") {

# 	rcpp <- self$rcpp()
# 	if(!missing(runs)) {
# 		self$runs <- runs
# 		rcpp$nb_runs <- self$runs
# 	} 
# 	rcpp$set_mode(mode)
# 	rcpp$make_lists()
# 	cachelists <- rcpp$get_lists()

# 	transform.as.data.frame <- function(cl,nms) {
# 		res <- list()
# 		if(length(cl>0)) for(term in 1:length(cl[[1]])) {
# 			res[[term]] <-list(before=list(),after=list())
# 			for(nm in nms[[term]]) {
# 				res[[term]]$before[[nm]] <- c()
# 				for(pt in 1:length(cl)) {
# 					if(length(cl[[pt]][[term]]$before)>0) for(j in 1:length(cl[[pt]][[term]]$before)) res[[term]]$before[[nm]] <- c(res[[term]]$before[[nm]],cl[[pt]][[term]]$before[[j]][[nm]])
# 					if(length(cl[[pt]][[term]]$after)>0) for(j in 1:length(cl[[pt]][[term]]$after)) res[[term]]$after[[nm]] <- c(res[[term]]$after[[nm]],cl[[pt]][[term]]$after[[j]][[nm]])
# 				}
# 			}
# 			res[[term]]$before <- as.data.frame(res[[term]]$before)
# 			res[[term]]$after <- as.data.frame(res[[term]]$after)
# 		}
# 		res
# 	}

# 	#This can be done directly in c++
# 	transform.as.list <- function(cl,nms) {
# 		res <- list()
# 		if(length(cl>0)) for(term in 1:length(cl[[1]])) {
# 			res[[term]] <-list(before=list(),after=list())
			 
# 			for(pt in 1:length(cl)) {
# 				if(length(cl[[pt]][[term]]$before)>0) for(j in 1:length(cl[[pt]][[term]]$before)) res[[term]]$before <- c(res[[term]]$before,cl[[pt]][[term]]$before[j])
# 				if(length(cl[[pt]][[term]]$after)>0) for(j in 1:length(cl[[pt]][[term]]$after)) res[[term]]$after <- c(res[[term]]$after,cl[[pt]][[term]]$after[j])
# 			}
			
# 		}
# 		res
# 	}

# 	names.cexprs <- lapply(self$interMngr$terms,function(term) names(term$mngr$local$cexprs$term))

# 	self$cexprs.cachelists <- switch(transform,
# 	nothing= cachelists,
# 	as.data.frame=list(first=transform.as.data.frame(cachelists$first,names.cexprs),second=transform.as.data.frame(cachelists$second,names.cexprs)),
# 	as.list=list(first=transform.as.list(cachelists$first,names.cexprs),second=transform.as.list(cachelists$second,names.cexprs))
# 	)

# 	self$cexprs.cachelists

# }