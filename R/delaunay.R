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
			self$.last.points=vertices(self,"save") #self$rcpp()$vertices()
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
	# if missing pts, pts is completed with ...
	if(missing(pts)) pts <- data.frame(...)

	# VERY IMPORTANT: to renew Delaunay object
	# RMK: ultimately, this can save the NN-graph if "id" is saved and k-NN are saved too => I like it! 
	if(!is.null(attr(pts,"saved.infos"))) { 
		for(i in 1:NROW(pts)) obj$rcpp()$insert_one_with_info(unlist(pts[i,]), attr(pts,"saved.infos")[[i]])
		return()
	}

	# The rest is for regular insertion!

	# insertion one by one (maybe too slow) but with info (i.e. mark)
	# mark is considered here as unidimensional and all vertices have values for each mark
	if(is.data.frame(pts)) {
		if(all((coord <- c("x","y","z")) %in% names(pts))) {
			if(obj$dim == 3) coord <- c("x","y") # "z" is then a mark!
		} else if(all((coord <- c("x","y")) %in% names(pts))) {
			if(obj$dim == 3) stop("z coordinate is missing!")
		} else stop("No coordinates provided!")
		info <- pts[,-which(coord %in% names(pts)),drop=FALSE]
		coord <- pts[,coord,drop=FALSE]
		for(i in 1:NROW(coord)) obj$rcpp()$insert_one_with_info(unlist(coord[i,]),as.list(info[i,,drop=FALSE]))
	} else {
		tmp <- cbind(...)
		if(NCOL(tmp)>1) pts <- cbind(pts,tmp)
		if(!is.matrix(pts)) pts <- matrix(pts,nrow=obj$dim)
		if(NCOL(pts)!=obj$dim && NROW(pts)==obj$dim) pts <- t(pts)
		if(obj$dim==2) obj$point.index <- obj$rcpp()$insert(pts[,1],pts[,2])
		else if(obj$dim==3) obj$point.index <- obj$rcpp()$insert(pts[,1],pts[,2],pts[,3])
		obj$points <- pts
		## special save call for persistency
	}
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

vertices.Delaunay <- function(obj,mode=c("default","incident","dual","all","save"),pt=NULL) {
	switch(match.arg(mode),
		incident=if(!is.null(pt)) obj$rcpp()$incident_vertices(pt) else NULL,
		dual=obj$rcpp()$dual_vertices(),
		all={
			data.frame(tmp<-obj$rcpp()$vertices()) -> tmp3
			names(tmp3) <- c("x","y","z")[1:ncol(tmp)]
			obj$rcpp()$vertices_infos() -> tmp2
			if(length(names(tmp2[[1]]))>0) {
				if(length(tmp2[[1]])==1) {#specific treatment for length 1
					tmp22 <- data.frame(unlist(tmp2))
					names(tmp22) <- names(tmp2[[1]])
				} else tmp22 <- as.data.frame(t(sapply(tmp2,function(e) e[names(tmp2[[1]])])))
				cbind(tmp3,tmp22) -> tmp3
				names(tmp3) <- c(c("x","y","z")[1:ncol(tmp)],names(tmp2[[1]]))
			}
			tmp3
		},
		save={
			data.frame(tmp<-obj$rcpp()$vertices()) -> tmp3
			names(tmp3) <- c("x","y","z")[1:ncol(tmp)]
			attr(tmp3,"saved.infos") <- obj$rcpp()$vertices_infos()
			tmp3
		},
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

length.Delaunay <- function(obj) NROW(vertices(obj))
