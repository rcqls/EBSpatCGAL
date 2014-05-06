
Single.ParameterMngr <- function(self) {
	res <- self$rcpp()$single
	if(res != self$interMngr$single) warning("single is not synchronized!")
	res
}
"Single<-.ParameterMngr" <- function(self,value) {
	self$interMngr$single <- value
	rcpp <- self$rcpp()
	rcpp$single <- value
	self
}


params.ParameterMngr <- function(self,...,params) {
	if(missing(params)) params <- list(...)
	if(!is.list(params)) stop("params have to be a list!")
	if(length(params)==0) return(params(self$interMngr))

	if("Single" %in% names(params)) Single(self) <- params$Single
	if("single" %in% names(params)) Single(self) <- params$single
	do.call("params",c(list(self$interMngr),params))
}
