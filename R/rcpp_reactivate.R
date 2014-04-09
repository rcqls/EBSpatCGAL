PersistentRcppObject <- function(obj,initialize_body,finalize_body) {
	if(is.environment(obj) && exists(".rcpp",envir=obj)) { 
		
		# use obj$rcpp() to get the obj$.rcpp object
		obj$rcpp <- function() {
			if(EBSpatCGAL:::is_xptr_null(obj$.rcpp$.pointer)) {
				obj$initialize() # build .rcpp
			}
			return(obj$.rcpp)
		}

		## To change this if not available on system
		obj$rcpp_id <- system("uuidgen",TRUE)

		obj$initialize <- function() {}
		body(obj$initialize) <- substitute(initialize_body)
		body(obj$initialize)[[length(body(obj$initialize))+1]] <- substitute(reg.finalizer(obj,obj$finalize,TRUE))


		obj$finalize <- function(obj) {}
		body(obj$finalize) <- substitute(finalize_body)

		# first registration of the finalizer
		reg.finalizer(obj,obj$finalize,TRUE)

	} else {
		stop("Not a proper object to make persistent!")
	}
	
	return(obj)
}

rdsFile.PersistentRcppObject <- function(obj) paste(".PersistentRcppObject",obj$rcpp_id,".rds",sep="")

save.PersistentRcppObject <- function(obj,...) {
	vars <- list(...)
	saveRDS(vars,rdsFile.PersistentRcppObject(obj))
}

load.PersistentRcppObject <- function(obj) {
    readRDS(rdsFile.PersistentRcppObject(obj))
}

