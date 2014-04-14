## Notice: self is required inside constructor providing to avoid first argument in method!
## in the "ruby" way! (as in rcpp_persistent_object.R).

Delaunay <- function(dim=2) {
	## 1) create an environment (required use of self)
	self <- newEnv(Delaunay,dim=dim) # self can now be used inside method defined inside this constructor!
	
	## 3) Managing persistency
	## Rmk: no function but bloc with 
	RcppPersistentObject(self, new = { # this is required
			switch(paste("dim",self$dim,sep=""),
			dim2={
				class(self) <- unique(c("Delaunay_2d",class(self)))
				new(Delaunay2)
			},
			dim3={
				class(self) <- unique(c("Delaunay_3d",class(self)))
				new(Delaunay3)
			})
		},
		renew = { # the new method is called before this optional method
			cat("Delaunay object Rcpp-reinitialized!\n")
			insert(self,self$.last.points)
		},
		save = { # this optional method has to be called whenever you need to update data used in the renew process
			self$.last.points=self$rcpp()$vertices()
			cat("Delaunay object Rcpp-saved!\n")
		}
	)

	## 4) return the created object
	return(self)
}

Delaunay_2d <- function(...) Delaunay(dim=2,...)
Delaunay_3d <- function(...) Delaunay(dim=3,...)


## in the R way, I prefer to use obj instead of 
insert.Delaunay <- function(obj,pts,...) {
	tmp <- cbind(...)
	if(NCOL(tmp)>1) pts <- cbind(pts,tmp)
	if(!is.matrix(pts)) pts <- matrix(pts,nrow=obj$dim)
	if(NCOL(pts)!=obj$dim && NROW(pts)==obj$dim) pts <- t(pts)
	if(obj$dim==2) obj$point.index <- obj$rcpp()$insert(pts[,1],pts[,2])
	else if(obj$dim==3) obj$point.index <- obj$rcpp()$insert(pts[,1],pts[,2],pts[,3])
	obj$points <- pts
	## special save call for persistency
	obj$save()
	return(invisible())
}

print.Delaunay <- function(obj) {
	print.default(obj$rcpp())
	print.default(obj)
}
