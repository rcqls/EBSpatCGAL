# Example:
# lc <- ListsCache(del2 ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => params(lc,th=c(2,4)) and update(lc,current=del2)


ListsCache <-function(model,...,runs=1000,domain=c(-350,-350,350,350)) {
	formMngr <- ComponentFunctionalFormulaManager()
	formula(formMngr,model,local=TRUE)
	forms <- as.list(substitute(c(...)))[-1]
	for(f in forms) formula(formMngr,f,local=TRUE) # First consider only the main case 
	print(formula(formMngr))
	self <- newEnv(ListsCache,forms=forms,formMngr=formMngr,interMngr=InteractionMngr(formMngr$func,check.params=FALSE),runs=runs,domain=domain)
	# the first formula is supposed to model formula with response in the left part of the formula
	self$response <- if(formMngr$formulas[[1]][[1]]==as.name("~") && length(formMngr$formulas[[1]])==3) formMngr$formulas[[1]][[2]] else NULL
	if(!is.null(self$response)) {
		current <- try(eval.parent(self$response))
		if(inherits(current,"try-error")) {
			warning("No proper response in ListsCache!")
			self$struct <- NULL
		} else update(self,current)
	} else self$dim <- 2 # default value if no answer updatable if struct changed

	RcppPersistentObject(self,new = { 
		if(is.null(self$struct)) {
			## TODO: initialilize ListsCache without del2
			## Maybe create one! 
		} else {	
			rcpp <- new(ListsCacheCpp,terms(self$interMngr),self$domain[1:self$dim],self$domain[self$dim+(1:self$dim)])
			rcpp$nb_runs <- self$runs
			if(!is.null(self$interMngr$mark.name)) {
				rcpp$marked(TRUE)
				rcpp$mark_expr(self$interMngr$mark.expr)
				tmp <- as.list(rep(NA,length(self$interMngr$mark.name)))
				names(tmp) <- self$interMngr$mark.name
				self$struct$rcpp()$update_infinite_vertex_info(tmp)
			} else rcpp$marked(FALSE)
			# important for renew process!
			if(!is.null(self$struct)) update(self$interMngr,self$struct)
			rcpp
		} 
	})
	self
}

## This delegates change of parameter value to InteractionManager 
params.ListsCache <- function(self,...) params(self$interMngr,...)

##########################################################################
# RMK: Interaction C++ object knows about STRUCT class via its first term 
# so no need to communicate the graph structure to ListsCache.
# This method is in charge to communicate the struct to all the terms of 
# the interaction manager.
##########################################################################
update.ListsCache <- function(self,current) {
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


## RMK: If you have a single configuration of point you need to complete 
## this data with an implicit Simulable object! => TODO!!!
run.ListsCache <- function(self,current,...) {
	params(self,...)
	if(!missing(current)) {
		if(inherits(current,"Simulable")) update(self,current) 
		else cat("WARNING: object not of class Simulable!\n")
	}
	if(!is.null(self$struct)) {
		self$rcpp()$eval_exprs()
	}
}


### NOT VERY USEFUL NOW sine everything is done in C++
### Maybe, can help for debugging
get.ListsCache <- function(self,mode=1,runs,transform="nothing") {

	rcpp <- self$rcpp()
	if(!missing(runs)) {
		self$runs <- runs
		rcpp$nb_runs <- self$runs
	} 
	rcpp$set_mode(mode)
	rcpp$make_lists()
	cachelists <- rcpp$get_lists()

	transform.as.data.frame <- function(cl,nms) {
		res <- list()
		if(length(cl>0)) for(term in 1:length(cl[[1]])) {
			res[[term]] <-list(before=list(),after=list())
			for(nm in nms[[term]]) {
				res[[term]]$before[[nm]] <- c()
				for(pt in 1:length(cl)) {
					if(length(cl[[pt]][[term]]$before)>0) for(j in 1:length(cl[[pt]][[term]]$before)) res[[term]]$before[[nm]] <- c(res[[term]]$before[[nm]],cl[[pt]][[term]]$before[[j]][[nm]])
					if(length(cl[[pt]][[term]]$after)>0) for(j in 1:length(cl[[pt]][[term]]$after)) res[[term]]$after[[nm]] <- c(res[[term]]$after[[nm]],cl[[pt]][[term]]$after[[j]][[nm]])
				}
			}
			res[[term]]$before <- as.data.frame(res[[term]]$before)
			res[[term]]$after <- as.data.frame(res[[term]]$after)
		}
		res
	}

	#This can be done directly in c++
	transform.as.list <- function(cl,nms) {
		res <- list()
		if(length(cl>0)) for(term in 1:length(cl[[1]])) {
			res[[term]] <-list(before=list(),after=list())
			 
			for(pt in 1:length(cl)) {
				if(length(cl[[pt]][[term]]$before)>0) for(j in 1:length(cl[[pt]][[term]]$before)) res[[term]]$before <- c(res[[term]]$before,cl[[pt]][[term]]$before[j])
				if(length(cl[[pt]][[term]]$after)>0) for(j in 1:length(cl[[pt]][[term]]$after)) res[[term]]$after <- c(res[[term]]$after,cl[[pt]][[term]]$after[j])
			}
			
		}
		res
	}

	names.cexprs <- lapply(self$interMngr$terms,function(term) names(term$mngr$local$cexprs$term))

	self$cexprs.cachelists <- switch(transform,
	nothing= cachelists,
	as.data.frame=list(first=transform.as.data.frame(cachelists$first,names.cexprs),second=transform.as.data.frame(cachelists$second,names.cexprs)),
	as.list=list(first=transform.as.list(cachelists$first,names.cexprs),second=transform.as.list(cachelists$second,names.cexprs))
	)

	self$cexprs.cachelists

}