Domain <-function(left=c(-350,-350),right=c(350,350)) {
	self <- newEnv(Domain,left=left,right=right)

	RcppPersistentObject(self,new = { 
		rcpp <- new(DomainCpp,self$left,self$right)
		rcpp 
	})
	self
}

length.Domain <- function(self) self$rcpp()$get_size() #area or volume

dim.Domain <- function(self) self$rcpp()$get_dim()

run.Domain <- function(self) self$rcpp()$pick() #choose a point randomly inside domain

"%contains%.Domain" <- function(self,pt) self$rcpp()$contains(pt)