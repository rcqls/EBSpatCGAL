Pseudo <- function(model,runs=1000,domain=c(-350,-350,350,350)) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,runs=runs,domain=domain)
	class(self) <- c("Pseudo",class(self))
	formula(self,expression(first=exp(-(.V)),second=-(.V)))
	self$contrast <- ContrastOptim(self)

	# define the optim.function method
	self$optim.function <- function(param) {
		self$optim.param(param)
		run.GNZCache(self) -> tmp
		print(tmp$first-tmp$second)
		tmp$first-tmp$second
	}

	self$optim.param <- function(param) {
		do.call("params",c(list(self),by(self$param.vect2list,param)))
	}

	self
}

# optim.options=list(method=,verbose=,...) (see run.ContrastOptim)
run.Pseudo <- function(self,...,fixed,optim.options=list()) {
	# structure of the converter parameter for the ContrastOptim  
	if(is.null(self$param.vect2list)) {
		if(length(list(...))==0) stop("Need to initialize parameters values first!")
		params(self,...)
		self$param.vect2list<- Vector2ListConverter(sapply(params(self),function(e) sapply(e,length)))
	}

	par0 <- unlist(params(self))

	# delegate the run method to self$contrast 
	if(missing(fixed)) do.call("run",c(list(self$contrast,par0),optim.options))
	else do.call("run",c(list(self$contrast,par0,fixed=fixed),optim.options))

	# return the result
	params(self)
}




