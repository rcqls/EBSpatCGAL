Pseudo <- function(model,runs=1000,domain=c(-350,-350,350,350),exponential=FALSE) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,runs=runs,domain=domain)
	attr(self,"expo") <- exponential
	class(self) <- c("Pseudo",class(self))
	formula(self,expression(first=exp(-(.V)),second=-(.V)))
	self$contrast <- ContrastOptim(self)

	if(exponential) {
		self$optim.update <- function() {
			self$rcpp()$get_cexprs_lists() -> cexprs
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
			res$second <- apply(res$second,2,sum)/self$domain.volume
			self$optim.statex <- res
		}

		# define the optim.function method
		self$optim.function <- function(param) {
			mean(exp(-self$optim.statex$first%*%param))+sum(self$optim.statex$second*param)
		}

		self$optim.gradient <- function(param) {
			self$optim.statex$second-apply(self$optim.statex$first*as.vector(exp(-self$optim.statex$first%*%param)),2,mean)
		}
	} else {
		# define the optim.function method
		self$optim.function <- function(param) {
			self$optim.param(param)
			run.GNZCache(self) -> tmp
			print(c(param=param,result=tmp$first-tmp$second))
			tmp$first-tmp$second
		}

		self$optim.param <- function(param) {
			do.call("params",c(list(self),by(self$param.vect2list,param)))
		}
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

	par0 <- if(length(list(...))==0) self$contrast$par else unlist(params(self))

	# exponential mode: update cache is not managed with run.GNZCache
	if(attr(self,"expo")) {
		if(self$to_make_lists) {
			cat("Please be patient: update of caches -> ")
			self$rcpp()$make_lists()
			
			self$optim.update() #update self$optim.statex

			self$to_make_lists <- FALSE
			cat("done! \n")
		}
	}

	# delegate the run method to self$contrast 
	if(missing(fixed)) do.call("run",c(list(self$contrast,par0),optim.options))
	else do.call("run",c(list(self$contrast,par0,fixed=fixed),optim.options))

	# return the result
	by(self$param.vect2list,unlist(self$contrast$par)) #params(self)
}




