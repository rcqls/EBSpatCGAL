# Model exponential family required here!
# WARNING: no redefinition of formula and params inherited from GNZCache required!
TKInverse <- function(model,domain=Domain(c(-350,-350),c(350,350)),runs=NULL,grid=NULL) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,domain=domain,runs=runs,grid=grid)
	class(self) <- c("TKInverse","Contrast",class(self))
	attr(self,"statex") <- TRUE
	self$contrast <- ContrastOptim(self)

	 
	self$optim.statex_update <- function() {
		res <- terms(self)
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