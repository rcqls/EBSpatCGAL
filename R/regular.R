Regular <- function(dim=2) {

	# 0) use of vertices method may be wrong because of spatstat
	.compatibility.with.spatstat() #where is the best place????

	## 1) create an environment (required use of self)
	self <- newEnv(Regular,GraphWithDual,Simulable,dim=dim) # self can now be used inside method defined inside this constructor!
	
	## 3) Managing persistency
	## Rmk: no function but block with 
	RcppPersistentObject(self, new = { # this is required
			switch(paste("dim",self$dim,sep=""),
			dim2={
				class(self) <- unique(c("Regular_2d",class(self)))
				new(Regular2)
			},
			dim3={
				class(self) <- unique(c("Regular_3d",class(self)))
				new(Regular3)
			})
		},
		renew = { # the new method is called before this optional method
			cat("Regular object Rcpp-reinitialized!\n")
			insert(self,self$.last.points)
		},
		save = { # this optional method has to be called whenever you need to update data used in the renew process
			self$.last.points=vertices(self,"save") #self$rcpp()$vertices()
			self$uid <- runif(1) #random number to identify an update of self (likewise uuid)
			#cat("Regular object Rcpp-saved!\n")
		}
	)

	## 4) return the created object
	return(self)
}

Regular_2d <- function(...) Regular(dim=2,...)
Regular_3d <- function(...) Regular(dim=3,...)


insert.Regular <- function(obj,pts,...) {
	# if missing pts, pts is completed with ...
	if(missing(pts)) pts <- data.frame(...)

	# VERY IMPORTANT: to renew Delaunay object
	# RMK: ultimately, this can save the NN-graph if "id" is saved and k-NN are saved too => I like it! 
	# if(!is.null(attr(pts,"saved.infos"))) { 
	# 	for(i in 1:NROW(pts)) obj$rcpp()$insert(unlist(pts[i,]))
	# 	return()
	# }

	# The rest is for regular insertion!

	# insertion one by one (maybe too slow) but with info (i.e. mark)
	# mark is considered here as unidimensional and all vertices have values for each mark
	if(is.data.frame(pts)) {
		if(all((coord <- c("x","y","z","w")) %in% names(pts))) {
			if(obj$dim == 2) coord <- c("x","y","w") # "z" is then a mark!
		} else if(all((coord <- c("x","y","w")) %in% names(pts))) {
			if(obj$dim == 3) stop("z coordinate is missing!")
		} else stop("No coordinates provided!")
		#info <- pts[,-which(coord %in% names(pts)),drop=FALSE]
		coord <- pts[,coord,drop=FALSE]
		for(i in 1:NROW(coord)) obj$rcpp()$insert_one(unlist(coord[i,]))
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

vertices.Regular <- function(obj,mode=c("default","incident","dual","all","save","info"),pt=NULL) {
	switch(match.arg(mode),
		incident=if(!is.null(pt)) obj$rcpp()$incident_vertices(pt) else NULL,
		dual=obj$rcpp()$dual_vertices(),
		# all={
		# 	data.frame(tmp<-obj$rcpp()$vertices()) -> tmp3
		# 	names(tmp3) <- c("x","y","z")[1:ncol(tmp)]
		# 	obj$rcpp()$vertices_infos() -> tmp2
		# 	if(length(names(tmp2[[1]]))>0) {
		# 		if(length(tmp2[[1]])==1) {#specific treatment for length 1
		# 			tmp22 <- data.frame(unlist(tmp2))
		# 			names(tmp22) <- names(tmp2[[1]])
		# 		} else tmp22 <- as.data.frame(t(sapply(tmp2,function(e) e[names(tmp2[[1]])])))
		# 		cbind(tmp3,tmp22) -> tmp3
		# 		names(tmp3) <- c(c("x","y","z")[1:ncol(tmp)],names(tmp2[[1]]))
		# 	}
		# 	tmp3
		# },
		# info={
		# 	obj$rcpp()$vertices_infos() -> tmp2
		# 	if(length(names(tmp2[[1]]))>0) {
		# 		if(length(tmp2[[1]])==1) {#specific treatment for length 1
		# 			tmp22 <- data.frame(unlist(tmp2))
		# 			names(tmp22) <- names(tmp2[[1]])
		# 		} else tmp22 <- as.data.frame(t(sapply(tmp2,function(e) e[names(tmp2[[1]])])))
		# 	} else tmp22 <- NULL
		# 	tmp22
		# },
		info=NULL,
		save={
			data.frame(tmp<-obj$rcpp()$weighted_vertices()) -> tmp3
			names(tmp3) <- c("x","y","z","w")[1:ncol(tmp)]
			attr(tmp3,"saved.infos") <- NULL #obj$rcpp()$vertices_infos()
			tmp3
		},
		obj$rcpp()$vertices()
	)
}