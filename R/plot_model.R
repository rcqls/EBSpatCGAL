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

"%with%.Vertex2d" <- "%with%.Vertex3d" <-
"%with%.Segment2d" <- "%with%.Segment3d" <- 
"%with%.Facet3d" <-function(obj,expr) {
	expr <- substitute(expr)
	obj$expr <- expr
	obj
}

elements.Vertex2d <- function(obj) {
	points <- switch(obj$type,
		delaunay= {
			if(is.null(obj$pt)) vertices(obj$parent)
			else vertices(obj$parent,"incident",obj$pt)
		},
		voronoi= vertices(obj$parent,"dual")[,1:2],
	)
	if(!is.null(obj$expr)) {
		points[apply(points,1,function(e) 
			eval(obj$expr,list(
				length=0 # TODO
				)
			)
		),]
	} else points
}

print.Vertex2d <- plot.Vertex2d <- function(obj) {
	pts <- elements(obj)
	#print(pts);print(c(list(pts),obj$attr))
	do.call("points",c(list(pts),obj$attr))
	Chainable.Scene()
}

elements.Segment2d <- function(obj) {
	edges <- switch(obj$type,
		delaunay= {
			if(is.null(obj$pt)) edges(obj$parent)
			else if(length(obj$pt)==1)  edges(obj$parent,"incident",as.integer(obj$pt))
			else if(length(obj$pt)==2)   edges(obj$parent,"conflicted",obj$pt)
		},
		voronoi= edges(obj$parent,"dual")
	)
	if(!is.null(obj$expr)) {
		edges[apply(edges,1,function(e) 
			eval(obj$expr,list(
				length=sqrt((e[3]-e[1])^2 + (e[4]-e[2])^2))
			)
		),]
	} else edges
	
}

print.Segment2d <- plot.Segment2d <- function(obj) {
	edges <- elements(obj)
	#print(pts);print(c(list(pts),obj$attr))
	do.call("segments",c(list(edges[,1],edges[,2],edges[,3],edges[,4]),obj$attr))
	Chainable.Scene()
}

print.Vertex3d <- plot.Vertex3d <- function(obj) {
	if(obj$type=="delaunay") pts <- obj$parent$rcpp()$vertices()
	else if(obj$type=="voronoi") pts <- obj$parent$rcpp()$dual_vertices()[,1:3]
	else return()
	#print(pts);print(c(list(pts),obj$attr))
	cmd <- if(!is.null(obj$attr$radius)) "spheres3d" else "points3d"  
	do.call(cmd,c(list(pts),obj$attr))
	Chainable.Scene()
}

elements.Segment3d <- function(obj) {
	# TODO: not in the same format as Segment2d
	if(obj$type=="delaunay") edges <- obj$parent$rcpp()$edges()
	else if(obj$type=="voronoi") edges <- obj$parent$rcpp()$dual_edges()
	if(!is.null(obj$expr)) {
		edges[apply(edges,1,function(e) 
			eval(obj$expr,list(length=sqrt((e[4]-e[1])^2 + (e[5]-e[2])^2 + (e[6]-e[3])^2)))
		),]
	} else edges
	
}

print.Segment3d <- plot.Segment3d <- function(obj) {
	edges <- t(matrix(t(elements(obj)),nr=3)) #treatment specific to rgl
	#print(pts);print(c(list(pts),obj$attr))
	do.call("segments3d",c(list(edges),obj$attr))
	Chainable.Scene()
}

elements.Facet3d <- function(obj) {
	facets <- t(matrix(t(obj$parent$rcpp()$facets()),nr=3))
	if(!is.null(obj$expr)) {
		facets[apply(facets,1,function(e) 
			eval(obj$expr,list(
				area=0 #TODO
				)
			)
		),]
	} else facets
}

print.Facet3d <- plot.Facet3d <- function(obj) {
	facets <- elements(obj)
	do.call("triangles3d",c(list(facets),obj$attr))
	Chainable.Scene()
}