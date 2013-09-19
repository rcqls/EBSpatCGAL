points.Delaunay <- function(obj,type=c("delaunay","voronoi"),...) {
	type <- match.arg(type)
	update(obj) #to avoid NULL xptr
	if(obj$dim==2) res <- CqlsObj(Vertex2d,type=type) 
	else if(obj$dim==3) res <- CqlsObj(Vertex3d,type=type)
 	res$obj <- obj
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(res$type,delaunay="blue",voronoi="red")
	res
}

lines.Delaunay <- function(obj,type=c("delaunay","voronoi"),...) {
	type <- match.arg(type)
	update(obj) #to avoid NULL xptr
	res <-  if(obj$dim==2) res <- CqlsObj(Segment2d,type=type) 
	else if(obj$dim==3) res <- CqlsObj(Segment3d,type=type)
	res$obj <- obj
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(type,delaunay="blue",voronoi="red")
	res
}

facets.Delaunay <- function(obj,...) {
	update(obj) #to avoid NULL xptr
	if(obj$dim==3) res <- CqlsObj(Facet3d)
	else return(NULL)
	res$obj <- obj
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- "blue"
	if(is.null(res$attr$alpha)) res$attr$alpha <- 0.1
	res
}

plot.Vertex2d <- function(obj) {
	update(obj$obj) #to avoid NULL xptr
	if(obj$type=="delaunay") pts <- obj$obj$graph$vertices()
	else if(obj$type=="voronoi") pts <- obj$obj$graph$dual_vertices()[,1:2]
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	do.call("points",c(list(pts),obj$attr))
}

plot.Segment2d <- function(obj) {
	update(obj$obj) #to avoid NULL xptr
	if(obj$type=="delaunay") edges <- obj$obj$graph$edges()
	else if(obj$type=="voronoi") edges <- obj$obj$graph$dual_edges()
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	do.call("segments",c(list(edges[,1],edges[,2],edges[,3],edges[,4]),obj$attr))
}

plot.Vertex3d <- function(obj) {
	obj$obj <- update(obj$obj) #to avoid NULL xptr
	if(obj$type=="delaunay") pts <- obj$obj$graph$vertices()
	else if(obj$type=="voronoi") pts <- obj$obj$graph$dual_vertices()[,1:3]
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	cmd <- if(!is.null(obj$attr$radius)) "spheres3d" else "points3d"  
	do.call(cmd,c(list(pts),obj$attr))
}

plot.Segment3d <- function(obj) {
	update(obj$obj) #to avoid NULL xptr
	if(obj$type=="delaunay") edges <- t(matrix(t(obj$obj$graph$edges()),nr=3))
	else if(obj$type=="voronoi") edges <- t(matrix(t(obj$obj$graph$dual_edges()),nr=3))
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	do.call("segments3d",c(list(edges),obj$attr))
}

plot.Facet3d <- function(obj) {
	update(obj$obj) #to avoid NULL xptr
	facets <- t(matrix(t(obj$obj$graph$facets()),nr=3))
	do.call("triangles3d",c(list(facets),obj$attr))
}