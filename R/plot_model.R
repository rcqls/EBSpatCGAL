points.Delaunay <- function(obj,type=c("delaunay","voronoi"),pt=NULL,...) {
	type <- match.arg(type)
	if(obj$dim==2) res <- CqlsObj(Vertex2d,type=type) 
	else if(obj$dim==3) res <- CqlsObj(Vertex3d,type=type)
 	res$obj <- obj
 	res$pt <- pt
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(res$type,delaunay="blue",voronoi="red")
	res
}

lines.Delaunay <- function(obj,type=c("delaunay","voronoi"),pt=NULL,...) {
	type <- match.arg(type)
	if(obj$dim==2) res <- CqlsObj(Segment2d,type=type) 
	else if(obj$dim==3) res <- CqlsObj(Segment3d,type=type)
	res$obj <- obj
	res$pt <- pt
	res$attr <- list(...)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(type,delaunay="blue",voronoi="red")
	res
}

facets.Delaunay <- function(obj,...) {
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
	switch(obj$type,
		delaunay= {
			if(is.null(obj$pt)) pts <- obj$obj$rcpp()$vertices()
			else pts <- obj$obj$rcpp()$incident_vertices(obj$pt)
		},
		voronoi= pts <- obj$obj$rcpp()$dual_vertices()[,1:2],
		return()
	)
	#print(pts);print(c(list(pts),obj$attr))
	do.call("points",c(list(pts),obj$attr))
}

plot.Segment2d <- function(obj) {
	switch(obj$type,
		delaunay= {
			if(is.null(obj$pt)) edges <- obj$obj$rcpp()$edges()
			else if(length(obj$pt)==1) edges <- obj$obj$rcpp()$incident_edges(as.integer(obj$pt))
			else if(length(obj$pt)==2) {
				edges <- rbind(
					obj$obj$rcpp()$conflicted_faces(obj$pt)[,1:4],
					obj$obj$rcpp()$conflicted_faces(obj$pt)[,3:6],
					obj$obj$rcpp()$conflicted_faces(obj$pt)[,c(1:2,5:6)]

				)
			}
		},
		voronoi= edges <- obj$obj$rcpp()$dual_edges(),
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