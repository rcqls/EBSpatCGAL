# Model exponential family required here!
# WARNING: no redefinition of formula and params inherited from GNZCache required!
TKInverse <- function(model,runs=1000,domain=Domain(c(-350,-350),c(350,350))) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,runs=runs,domain=domain)
	class(self) <- c("TKInverse","Contrast",class(self))
	attr(self,"statex") <- TRUE
	self$contrast <- ContrastOptim(self)

	 
	self$optim.statex_update <- function(check=FALSE) {
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
		if(check) res2 <- res
		res$first <- apply(res$first,2,mean)
		self$optim.statex <- res
		if(check) return(res2)
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