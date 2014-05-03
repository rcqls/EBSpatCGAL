# Option I:
# gd <- SimGibbs(del2 ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => params(gd,th=c(2,4)) and update(gd,current=del2)
# run(gd) 					# as many times as desired
#
# Option II:
# gd <- SimGibbs(~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => struct to be specified later! 
# gd$nb_runs <- 1000 # optional since default value is 10000
# As an example, you want to change "th" parameter too
# run(gd,del2,th=c(3,4)) 	# then run(gd) as many times as desired
# or
# params(gd,th=c(3,4))
# run(gd,del2) 				# then run(gd) as many times as desired
# or 
# params(gd,th=c(3,4))
# gd$dom <- del2 
# run(gd) 					# as many times as desired


## TODO: domain would be included later in Domain object embedding Struct 
## playing the role of response in formula
SimGibbs <-function(model,runs=10000,domain=c(-350,-350,350,350)) {
	self <- newEnv(SimGibbs,interMngr=InteractionMngr(model),runs=runs,domain=domain)
	self$response <- self$interMngr$response
	if(!is.null(self$response)) {
		current <- try(eval.parent(self$response))
		if(inherits(current,"try-error")) {
			warning("No proper response in SimGibbs!")
			self$struct <- NULL
		} else update(self,current)
	} else self$dim <- 2 # default value if no answer updatable if struct changed

	RcppPersistentObject(self,new = { 
		if(is.null(self$struct)) {
			## TODO: initialilize SimGibbsDel(2|3)D without del2
			## Maybe create one! 
		} else {	
			## No more SimGibbsDel(2|3)D even if TermType depends on dimension
			##rcpp <- new(eval(parse(text=paste("SimGibbsDel",self$dim,"D",sep=""))),terms(self$interMngr),self$struct$rcpp(),self$domain[1:self$dim],self$domain[self$dim+(1:self$dim)])
			rcpp <- new(SimGibbsCpp,terms(self$interMngr),self$domain[1:self$dim],self$domain[self$dim+(1:self$dim)])
			rcpp$single <- self$interMngr$single
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

params.SimGibbs <- function(self,...) params(self$interMngr,...)


##########################################################################
# RMK: VERY IMPORTANT TRICK! 
# Interaction C++ object knows about STRUCT class via its first term 
# so no need to communicate the graph structure to SimGibbs.
# This method is in charge to communicate the struct to all the terms of 
# the interaction manager.
##########################################################################
update.SimGibbs <- function(self,current) {
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

run.SimGibbs <- function(self,current,...) {
	params(self,...)
	if(!missing(current)) {
		if(inherits(current,"Simulable")) update(self,current) 
		else cat("WARNING: object not of class Simulable!\n")
	}
	if(!is.null(self$struct)) {
		self$rcpp()$run()
		self$struct$save()
	}
}