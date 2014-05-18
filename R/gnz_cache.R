# Example:
# lc <- GNZCache(del2 ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => params(lc,th=c(2,4)) and update(lc,current=del2)



## ... or forms

GNZCache <-function(model,...,domain=Domain(c(-350,-350),c(350,350)),runs=NULL,grid=NULL,forms) {
	formMngr <- ComponentFunctionalFormulaManager()
	formula(formMngr,model,local=TRUE)
	if(missing(forms)) forms <- as.list(substitute(c(...)))[-1]
	for(f in forms) formula(formMngr,f,local=TRUE) # First consider only the main case 
	#print(formula(formMngr))
	self <- newEnv(GNZCache,ParameterMngr,forms=forms,formMngr=formMngr,interMngr=InteractionMngr(formMngr$func,check.params=FALSE),domain=domain)
	# the first formula is supposed to model formula with response in the left part of the formula
	self$response <- if(formMngr$formulas[[1]][[1]]==as.name("~") && length(formMngr$formulas[[1]])==3) formMngr$formulas[[1]][[2]] else NULL
	if(!is.null(self$response)) {
		current <- try(eval.parent(self$response))
		if(inherits(current,"try-error")) {
			warning("No proper response in GNZCache!")
			self$struct <- NULL
		} else update(self,current)
	} else self$dim <- length(self$domain) # default value if no answer updatable if struct changed

	if(is.null(runs) && is.null(grid)) grid <- 10000

	if(!is.null(grid)) {
		self$mode <- 0
		if(length(grid)==1) grid <- rep(as.integer(grid^(1/self$dim)),self$dim)
		self$grid <- grid
	} else if(!is.null(runs)) {
		self$mode <- 1
		self$runs <- runs
	}

	self$domain.volume <- length(self$domain) #if(self$dim==2) (self$domain[3]-self$domain[1])*(self$domain[4]-self$domain[2])
							#else (self$domain[4]-self$domain[1])*(self$domain[5]-self$domain[2])*(self$domain[6]-self$domain[3])
			

	RcppPersistentObject(self,
		new = { 
			if(is.null(self$struct)) {
				## TODO: initialilize GNZCache without del2
				## Maybe create one! 
			} else {	
				rcpp <- new(GNZCacheCpp,terms(self$interMngr),self$domain$rcpp())
				rcpp$single <- self$interMngr$single
				rcpp$set_mode(self$mode)
				if(self$mode==0) self$domain$rcpp()$set_grid(self$grid) else rcpp$nb_runs <- self$runs

				#rcpp$set_sizes_for_interaction(c(as.integer(self$runs),as.integer(length(self$struct))))
				
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
				if(!is.null(self$exprs)) rcpp$set_exprs_for_interaction(self$exprs)
				rcpp
			}
		}
	)
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
update.GNZCache <- function(self,current,runs,grid) {
	if(!missing(current)) {
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
	if(!missing(runs) && (self$mode != 0 || !identical(self$runs,runs) )) {
		self$runs <- runs
		rcpp <- self$rcpp()
		rcpp$nb_runs <- self$runs
		rcpp$set_mode(self$mode <- 1)
		self$to_make_lists <- TRUE
	}

	if(!missing(grid) && (self$mode != 1 || !identical(self$grid,grid) ) ) {
		self$grid <- grid
		self$domain$rcpp()$set_grid(self$grid)
		self$rcpp()$set_mode(self$mode <- 0)
		self$to_make_lists <- TRUE
	}

}


## RMK: If you have a single configuration of points you need to complete 
## this data with an implicit Simulable object! => TODO!!!
run.GNZCache <- function(self,current,...,runs,grid,domain,form) {
	#if(attr(self,"statex")) stop("run.GNZCache not callable for exponential case!")
	params(self,...)

	rcpp <- self$rcpp()

	if(!missing(domain) && !identical(domain,self$domain)) {
		self$domain <- domain
		self$domain.volume <- length(self$domain) #if(self$dim==2) (self$domain[3]-self$domain[1])*(self$domain[4]-self$domain[2])
							#else (self$domain[4]-self$domain[1])*(self$domain[5]-self$domain[2])*(self$domain[6]-self$domain[3])
	
		rcpp$set_domain(self$domain$rcpp())
		self$to_make_lists <- TRUE
	}
	
	if(!missing(runs))  update(self,runs=runs)

	# && self$runs != runs) {
	# 	self$runs <- runs
	# 	rcpp$nb_runs <- self$runs
	# 	rcpp$set_sizes_for_interaction(c(as.integer(self$runs),as.integer(length(seq(self$struct)))))
	# 	# make_lists needed if change of runs
	# 	self$to_make_lists <- TRUE
	# }

	if(!missing(grid)) update(self,grid=grid)
	
	if(!missing(current) && (is.null(self$struct) || identical(self$struct,current))) {
		if(inherits(current,"Simulable")) {
			update(self,current=current)
			self$to_make_lists <- TRUE
		} else cat("WARNING: object not of class Simulable!\n")
	}

	# to update caches when struct has been udated too
	if(is.null(self$struct.uid) || self$struct$uid != self$struct.uid) {
		self$struct.uid <- self$struct$uid
		self$to_make_lists <- TRUE
	}

	if(self$to_make_lists) {
		cat("Please be patient: update of caches -> ")
		self$rcpp()$make_lists()
		#rcpp$set_sizes_for_interaction(c(as.integer(self$runs),rcpp$get_inside_number()))
		self$to_make_lists <- FALSE
		cat("done! \n")
	}


	if(!is.null(self$struct)) {

		if(!missing(form)) {
			exprs.save <- self$exprs
			formula(self,form) #self$exprs updated
		} else exprs.save <- NULL

		tmp <- rcpp$eval_exprs()
		
		self$last.eval_exprs <- tmp
		
		res <- list(first=tmp$first/self$runs,second=tmp$second/self$domain.volume)
		
		if(!missing(form) && !identical(self$exprs,exprs.save)) {# restore self$exprs
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

terms.GNZCache <- function(self,mode=c("statex","raw")) {
	if(!self$to_make_lists) {
		self$rcpp()$get_cexprs_lists() -> cexprs
		mode <- match.arg(mode)
		if(mode=="raw") return(cexprs)
		# first
		res <- list(first=NULL,second=NULL)
		for(type in names(res)) {#over the types
			for(pt in seq(cexprs[[type]])) {#over the points
				terms<-cexprs[[type]][[pt]]
				tmp <- 1
				for(iterm in 1:length(terms)) {#over the terms
					term <- terms[[iterm]]
					tmp2 <- 0
					for(i in seq(term$after)) {
						tmp2 <- tmp2 + unlist(term$after[[i]])
					}
					for(i in seq(term$before)) {
						tmp2 <- tmp2 - unlist(term$before[[i]])
					}
					tmp <- c(tmp,tmp2)
				}
				res[[type]]<- rbind(res[[type]],tmp)
			}
			dimnames(res[[type]]) <- list(1:nrow(res[[type]]),paste("s",1:ncol(res[[type]]),sep=""))
		}
		 
		res
		} else warning("cache lists not already done!")
}

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