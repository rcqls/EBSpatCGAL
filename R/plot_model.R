# default plot for Delaunay

plot.Delaunay <- plot.GraphWithDual <- function(obj,scene,...,plot=TRUE) {
	if(missing(scene)) {
		scene <- "delaunay"
	}
	# predefined scene
	if(is.character(scene)) {
		defined.scenes <- c("points","delaunay","voronoi")
		if(exists(".user.defined.scenes")) defined.scenes <- c(defined.scenes,.user.defined.scenes)
		sceneType <- match.arg(scene,defined.scenes)
		scene <- Scene()
		if(obj$dim==2) {
			scene$actors$main <- scene$actors$xlab <- scene$actors$ylab <- ""
			scene %<<% window(.del,main=main,xlab=xlab,ylab=ylab)
		} else {
			scene$actors$windowRect <- c(0,0,800,800)
			scene %<<% window(.del,windowRect=windowRect)
		}
		switch(sceneType,
			points={
				scene$actors$col <- "black"
				if(obj$dim==2) {
					scene %<<% points(.del,col=col)
				} else {
					scene$actors$radius <- 3 #to improve as a factor of diameter domain
					scene %<<% points(.del,col=col,radius=radius)
				}
			},
			delaunay={
				scene$actors$col <- "black"
				if(obj$dim==2) {
					scene %<<% lines(.del) %<<% points(.del,col=col)
				} else {
					scene$actors$radius <- 3 #to improve as a factor of diameter domain
					scene %<<% lines(.del) %<<% points(.del,col=col,radius=radius)
				}
			},
			voronoi={
				if(obj$dim==2) {
					scene %<<% points(.del,col=col) %<<% lines(.del,type="vor")
				} else {
					# TODO!!!!
					scene$actors$radius <- 3 #to improve as a factor of diameter domain
					scene %<<% points(.del,col=col,radius=radius) %<<% lines(.del,type="vor")
				}
			}
		)
	}

	## deal with marks! TODO: not the same mechanism as col (for example) argument for regular scene
	actors <- if(!is.null(vertices(obj,"info")->info)) eval(substitute(list(...)),info)
	 			else list(...)

	do.call("plot",c(list(scene,.del=obj),actors))
}

### elements

points.Delaunay <- points.GraphWithDual <- function(obj,type=c("delaunay","voronoi"),pt=NULL,when,...) {
	type <- match.arg(type)
	if(obj$dim==2) res <- newEnv(Vertex2d,type=type)
	else if(obj$dim==3) res <- newEnv(Vertex3d,type=type)
 	res$parent <- obj
 	res$parent.env <- parent.frame() #called in particular inside plot.Scene
 	res$pt <- pt
	res$attr <- substitute(list(...))
	if(!missing(when)) res$expr <- substitute(when)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(res$type,delaunay="blue",voronoi="red")
	if(is.null(res$attr$pch) && obj$dim==2) res$attr$pch <- 16 #switch(type,delaunay="blue",voronoi="red")
	res
}

lines.Delaunay <- lines.GraphWithDual <- function(obj,type=c("delaunay","voronoi"),pt=NULL,when,...) {
	type <- match.arg(type)
	if(obj$dim==2) res <- newEnv(Segment2d,type=type)
	else if(obj$dim==3) res <- newEnv(Segment3d,type=type)
	res$parent <- obj
	res$parent.env <- parent.frame() #called in particular inside plot.Scene
	res$pt <- pt
	res$attr <- list(...) #substitute(list(...))
	if(!missing(when)) res$expr <- substitute(when)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- switch(type,delaunay="blue",voronoi="red")
	res
}

facets.Delaunay <- facets.GraphWithDual <- function(obj,when,...) {
	if(obj$dim==3) res <- newEnv(Facet3d)
	else return(NULL)
	res$parent <- obj
	res$parent.env <- parent.frame() #called in particular inside plot.Scene
	res$attr <- list(...) #substitute(list(...))
	if(!missing(when)) res$expr <- substitute(when)
	## default color
	if(is.null(res$attr$col)) res$attr$col <- "blue"
	if(is.null(res$attr$alpha)) res$attr$alpha <- 0.1
	res
}

"%when%.Vertex2d" <- "%when%.Vertex3d" <-
"%when%.Segment2d" <- "%when%.Segment3d" <-
"%when%.Facet3d" <-function(obj,expr) {
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
			exprEnv <- list(
				length=0 # TODO
				),
			eval(obj$expr,exprEnv)
		),]
	} else points
}

print.Vertex2d <- plot.Vertex2d <- function(obj) {
	pts <- elements(obj)
	#print(pts);print(c(list(pts),obj$attr))

	vertices(obj$parent,"info") -> infos
	attrEnv <- if(obj$type=="delaunay" && !is.null(infos)) list2env(infos) else new.env()
	parent.env(attrEnv) <- obj$parent.env
	attrs<-eval(obj$attr,attrEnv)

	do.call("points",c(list(pts),attrs))
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

	attrEnv <- new.env()
	parent.env(attrEnv) <- obj$parent.env
	attrs<-eval(obj$attr,attrEnv)

	do.call("segments",c(list(edges[,1],edges[,2],edges[,3],edges[,4]), attrs))
	Chainable.Scene()
}

print.Vertex3d <- plot.Vertex3d <- function(obj) {
	if(obj$type=="delaunay") pts <- obj$parent$rcpp()$vertices()
	else if(obj$type=="voronoi") pts <- obj$parent$rcpp()$dual_vertices()[,1:3]
	else return()

	#print(pts);print(c(list(pts),obj$attr))

	vertices(obj$parent,"info") -> infos
	attrEnv <- if(obj$type=="delaunay" && !is.null(infos)) list2env(infos) else new.env()
	parent.env(attrEnv) <- obj$parent.env
	attrs<-eval(obj$attr,attrEnv)

	cmd <- if(!is.null(attrs$radius)) "spheres3d" else "points3d"
	do.call(cmd,c(list(pts),attrs))
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

	attrEnv <- new.env()
	parent.env(attrEnv) <- obj$parent.env
	attrs<-eval(obj$attr,attrEnv)

	do.call("segments3d",c(list(edges),attrs))
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
