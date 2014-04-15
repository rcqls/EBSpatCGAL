points.Delaunay <- function(obj,type=c("delaunay","voronoi"),pt=NULL,...) {
	type <- match.arg(type)
	if(obj$dim==2) res <- newEnv(Vertex2d,type=type) 
	else if(obj$dim==3) res <- newEnv(Vertex3d,type=type)
 	res$parent <- obj
 	res$pt <- pt
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(res$type,delaunay="blue",voronoi="red")
	res
}

lines.Delaunay <- function(obj,type=c("delaunay","voronoi"),pt=NULL,...) {
	type <- match.arg(type)
	if(obj$dim==2) res <- newEnv(Segment2d,type=type) 
	else if(obj$dim==3) res <- newEnv(Segment3d,type=type)
	res$parent <- obj
	res$pt <- pt
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(type,delaunay="blue",voronoi="red")
	res
}

facets.Delaunay <- function(obj,...) {
	if(obj$dim==3) res <- newEnv(Facet3d)
	else return(NULL)
	res$parent <- obj
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- "blue"
	if(is.null(res$attr$alpha)) res$attr$alpha <- 0.1
	res
}

plot.Vertex2d <- function(obj) {
	switch(obj$type,
		delaunay= {
			if(is.null(obj$pt)) pts <- vertices(obj$parent)
			else pts <- vertices(obj$parent,"incident",obj$pt)
		},
		voronoi= pts <- vertices(obj$parent,"dual")[,1:2],
		return()
	)
	#print(pts);print(c(list(pts),obj$attr))
	do.call("points",c(list(pts),obj$attr))
}

plot.Segment2d <- function(obj) {
	switch(obj$type,
		delaunay= {
			if(is.null(obj$pt)) edges <- edges(obj$parent)
			else if(length(obj$pt)==1) edges <- edges(obj$parent,"incident",as.integer(obj$pt))
			else if(length(obj$pt)==2)  edges <- edges(obj$parent,"conflicted",obj$pt)
		},
		voronoi= edges <- edges(obj$parent),
		return()
	)
	#print(pts);print(c(list(pts),obj$attr))
	do.call("segments",c(list(edges[,1],edges[,2],edges[,3],edges[,4]),obj$attr))
}

plot.Vertex3d <- function(obj) {
	if(obj$type=="delaunay") pts <- obj$obj$rcpp()$vertices()
	else if(obj$type=="voronoi") pts <- obj$obj$rcpp()$dual_vertices()[,1:3]
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	cmd <- if(!is.null(obj$attr$radius)) "spheres3d" else "points3d"  
	do.call(cmd,c(list(pts),obj$attr))
}

plot.Segment3d <- function(obj) {
	if(obj$type=="delaunay") edges <- t(matrix(t(obj$obj$rcpp()$edges()),nr=3))
	else if(obj$type=="voronoi") edges <- t(matrix(t(obj$obj$rcpp()$dual_edges()),nr=3))
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	do.call("segments3d",c(list(edges),obj$attr))
}

plot.Facet3d <- function(obj) {
	facets <- t(matrix(t(obj$obj$rcpp()$facets()),nr=3))
	do.call("triangles3d",c(list(facets),obj$attr))
}