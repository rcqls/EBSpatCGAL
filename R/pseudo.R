# WARNING: no redefinition of formula and params inherited from GNZCache required!
Pseudo <- function(model,domain=Domain(c(-350,-350),c(350,350)),runs=NULL,grid=NULL,exponential=FALSE) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,domain=domain,runs=runs,grid=grid)
	attr(self,"statex") <- exponential
	class(self) <- c("Pseudo","Contrast",class(self))
	if(!exponential) formula(self,expression(first=exp(-(.V)),second=-(.V)))
	self$contrast <- ContrastOptim(self)

	if(exponential) {
		self$optim.statex_update <- function() {
			res <- terms(self)
			self$optim.statex.second <- res$second
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



