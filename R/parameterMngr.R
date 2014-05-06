
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

params.ParameterMngr <- function(self,...) {
	if("Single" %in% names(list(...))) Single(self) <- list(...)$Single
	if("single" %in% names(list(...))) Single(self) <- list(...)$single
	params(self$interMngr,...)
}
