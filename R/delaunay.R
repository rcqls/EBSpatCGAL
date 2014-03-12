Delaunay <- function(dim=2) {
	obj <- CqlsObj(Delaunay)
	obj$dim <- dim
	obj$graph <- switch(paste("dim",dim,sep=""),
		dim2={
			class(obj) <- c("Delaunay_2d",class(obj))
			new(Delaunay2)
		},
		dim3={
			class(obj) <- c("Delaunay_3d",class(obj))
			new(Delaunay3)
		}
	)
	return(obj)
}

Delaunay_2d <- function(...) Delaunay(dim=2,...)
Delaunay_3d <- function(...) Delaunay(dim=3,...)

insert.Delaunay <- function(obj,pts,...) {
	tmp <- cbind(...)
	if(NCOL(tmp)>1) pts <- cbind(pts,tmp)
	if(!is.matrix(pts)) pts <- matrix(pts,nrow=obj$dim)
	if(NCOL(pts)!=obj$dim && NROW(pts)==obj$dim) pts <- t(pts)
	if(obj$dim==2) obj$point.index <- obj$graph$insert(pts[,1],pts[,2])
	else if(obj$dim==3) obj$graph$insert(pts[,1],pts[,2],pts[,3])
	obj$points <- pts
	return(invisible())
}

print.Delaunay <- function(obj) {
	update(obj)
	print.default(obj$graph)
	print.default(obj)
}

# use update(obj) instead obj to ensure xptr not NULL
update.Delaunay <- function(obj) {
	if(is_xptr_null(obj$graph$.pointer)) {
		obj$graph <-  switch(paste("dim",obj$dim,sep=""),
		dim2={
			new(Delaunay2)
		},
		dim3={
			new(Delaunay3)
		})
		insert(obj,obj$points)
	}
	return(invisible(obj))
}