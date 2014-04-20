## Notice: self is required inside constructor providing to avoid first argument in method!
## in the "ruby" way! (as in rcpp_persistent_object.R).

Delaunay <- function(dim=2) {
	## 1) create an environment (required use of self)
	self <- newEnv(Delaunay,Simulable,dim=dim) # self can now be used inside method defined inside this constructor!
	
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

## in the R way, I prefer to use obj instead of self
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

delete.Delaunay <- function(obj,pts,inside) {
	if(!missing(inside)) {
		obj$rcpp()$remove_inside(inside)
	} else {
		if(missing(pts)) pts <- seq(obj)
		for(i in pts) obj$rcpp()$remove_at_pos(i)
	}
	obj$save()
}

print.Delaunay <- function(obj) {
	print.default(obj$rcpp())
	print.default(obj)
}

## extract

vertices.Delaunay <- function(obj,mode=c("default","incident","dual"),pt=NULL) {
	switch(match.arg(mode),
		incident=if(!is.null(pt)) obj$rcpp()$incident_vertices(pt) else NULL,
		dual=obj$rcpp()$dual_vertices(),
		obj$rcpp()$vertices()
	)
}


edges.Delaunay <- function(obj,mode=c("default","incident","conflicted","dual"),pt=NULL) {
	switch(match.arg(mode),
	  	"incident"= if(length(pt)==1) obj$rcpp()$incident_edges(as.integer(pt)),
		"conflicted"= if(length(pt)==2) {
			confl_faces <- obj$rcpp()$conflicted_faces(pt)
			return(rbind(confl_faces[,1:4],confl_faces[,3:6],confl_faces[,c(1:2,5:6)]))
		},
		"dual"=obj$rcpp()$dual_edges(),
		obj$rcpp()$edges()
	)
}

seq.Delaunay <- function(obj) 1:NROW(vertices(obj))
