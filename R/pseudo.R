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



