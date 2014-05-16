rcpp <- function(self) {
	self$rcpp()
}


RcppPersistentObject <- function(self,new_body,renew_body={},save_body={}) {
	if(is.environment(self)) { 

		## rcpp recreate method
		self$new <- function() {}
		body(self$new) <- substitute(new_body)

		## First initialization
		self$.rcpp <- self$new()

		## rcpp recreate method
		self$renew <- function() {}
		body(self$renew) <- substitute(renew_body)
		 
		## rcpp save method
		self$save <- function() {}
		body(self$save) <- substitute(save_body)

		# use self$rcpp() to get the self$.rcpp object after a rcpp_renew call
		self$rcpp <- function(force=FALSE) {
			if(force || EBSpatCGAL:::is_xptr_null(self$.rcpp$.pointer)) {
				self$.rcpp <- self$new()
				self$renew() # build .rcpp
			}
			return(self$.rcpp)
		}

	} else {
		stop("Not a proper (environment) object to become persistent!")
	}
	
}
