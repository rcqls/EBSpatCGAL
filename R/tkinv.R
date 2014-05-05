# Model exponential family required here!
TKInverse <- function(model,runs=1000,domain=c(-350,-350,350,350)) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,runs=runs,domain=domain)
	class(self) <- c("TKInverse",class(self))
	
	self$contrast <- ContrastOptim(self)

	 
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
		res$first <- apply(res$first,2,mean)
		self$optim.statex <- res
	}

	# define the optim.function method
	self$optim.function <- function(param) {
		second <- apply(rep(exp(self$optim.statex$second%*%param),ncol(self$optim.statex$second))*self$optim.statex$second,2,sum)/self$domain.volume
		sum((self$optim.statex$first - second)^2)
	}

	self$optim.gradient <- function(param) {
		exp.param <- exp(self$optim.statex$second%*%param)
		contrast <- self$optim.statex$first - apply(rep(exp(self$optim.statex$second%*%param),ncol(self$optim.statex$second))*self$optim.statex$second,2,sum)/self$domain.volume
		-2*sapply(1:ncol(self$optim.statex$second),function(i) {
			tmp <- sum(contrast*apply(rep(exp.param*self$optim.statex$second[,i],ncol(self$optim.statex$second))*self$optim.statex$second,2,sum)/self$domain.volume)
		})
	}


	self
}

# optim.options=list(method=,verbose=,...) (see run.ContrastOptim)
run.TKInverse <- function(self,...,fixed,optim.options=list()) {
	# structure of the converter parameter for the ContrastOptim  
	if(is.null(self$param.vect2list)) {
		if(length(list(...))==0) stop("Need to initialize parameters values first!")
		params(self,...)
		self$param.vect2list<- Vector2ListConverter(sapply(params(self),function(e) sapply(e,length)))
	}

	par0 <- if(length(list(...))==0) self$contrast$par else unlist(params(self))

	# exponential mode: update cache is not managed with run.GNZCache
	 
	if(self$to_make_lists) {
		cat("Please be patient: update of caches -> ")
		self$rcpp()$make_lists()
		
		self$optim.update() #update self$optim.statex

		self$to_make_lists <- FALSE
		cat("done! \n")
	}
	

	# delegate the run method to self$contrast 
	if(missing(fixed)) do.call("run",c(list(self$contrast,par0),optim.options))
	else do.call("run",c(list(self$contrast,par0,fixed=fixed),optim.options))

	# return the result
	by(self$param.vect2list,unlist(self$contrast$par)) #params(self)
}
