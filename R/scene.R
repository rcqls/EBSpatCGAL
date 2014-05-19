Scene <- function(...) {
	obj <- newEnv(Scene)
	obj$actors <- new.env()
	actors <- list(...)
	for(nm in names(actors)) assign(nm,actors[[nm]],envir=obj$actors)
	obj$script <- list()
	obj
}

length.Scene <- function(obj) length(obj$script)

plot.Scene <- function(obj,subset,...) {
	list(...) -> actors
	if(length(actors)>0) {
		list2env(actors) -> actors
		parent.env(actors) <- obj$actors
	} else actors <- obj$actors
	# to extend the range of obj$actors to recognize object 
	parent.env(obj$actors) <- parent.env(environment())

	l <- if(missing(subset)) obj$script else obj$script[subset]
	for(comp in l) if(!is.null(comp)) {
		#print(comp);print(actors);print(ls(actors));print(ls(obj$actors))
		plot(eval(comp,envir=actors))
		#plot(comp)
	}
	Chainable.Scene()
}

"[[.Scene" <- function(obj,ref,eval=FALSE) {
	if(eval) {
		# to extend the range of obj$actors to recognize object 
		parent.env(obj$actors) <- parent.env(environment())
		eval(obj$script[[ref]],envir=obj$actors)
	} else obj$script[[ref]] #ref is integer or string
}

"[[<-.Scene" <- function(obj,ref,comp) {
	comp <- substitute(comp)
 	obj$script[[ref]] <- comp #ref is integer or string
 	obj
}

# extract some art of the scene
"[.Scene" <- function(obj,ref) {
	obj2 <- Scene()
	if(missing(ref)) obj2$script <- obj$script
 	else obj2$script <- obj$script[ref]
 	obj2
}


## to insert components
"[<-.Scene" <- function(obj,ref,inc,comp) {
	if(missing(inc)) inc <- 0
	if(missing(comp)) comp <- substitute(inc)
	else comp <- substitute(comp)
	tmp <- obj$script
	obj$script <- NULL
	if(is.character(ref)) ref <- which(ref %in% names(tmp))+inc
	if(ref>length(tmp)) ref <- length(tmp)+1
	if(ref==1) obj$script <- comp else obj$script <- c(tmp[1:(ref-1)],comp)
	if(ref<=length(tmp)) obj$script <- c(obj$script,tmp[ref:length(tmp)])
	obj
}

"%<<%.Scene" <- function(obj,comp) {
	comp <- substitute(comp)
	obj$script[[length(obj$script)+1]] <- comp 
	return(invisible(obj))
}

print.Scene <- function(obj,...) {
	print(list(actors=obj$actors,components=obj$script))
}

elements.Scene <- function(obj,i) elements(obj[[i,TRUE]])


#update.Scene <- function(obj,formula) {#formula allow to change the order of the element
#
#}

window.Delaunay <- function(obj,...) {
	bb <- apply(apply(vertices(obj),2,range),2,function(e) c(floor(e[1]),ceiling(e[2])))
	dim <- ncol(bb)
	if(dim==2) window2d(bb[,1],bb[,2],...)
	else if(dim==3) window3d(bb[,1],bb[,2],bb[,3],...)
}

window.Domain <- function(obj,...) {
	dim <- length(obj$left)
	if(dim==2) window2d(c(obj$left[1],obj$right[1]),c(obj$left[2],obj$right[2]),...)
	else if(dim==3) window3d(c(obj$left[1],obj$right[1]),c(obj$left[2],obj$right[2]),c(obj$left[3],obj$right[3]),...)
}

window2d <- function(xrange=c(0,1),yrange=c(0,1),main="",xlab="",ylab="",...) {
	res <- newEnv(Window2d)
	res$obj <- list(xrange,yrange)
	res$attr <- list(main="",xlab=xlab,ylab=ylab,...)
	res
}

plot.Window2d <- function(obj) {
	do.call("plot",c(list(obj$obj[[1]],obj$obj[[2]],type="n",asp=1),obj$attr))
	Chainable.Scene()
}

window3d <- function(xrange=c(0,1),yrange=c(0,1),zrange=c(0,1),...) {
	res <- newEnv(Window3d)
	res$obj <- list(xrange,yrange,zrange)
	res$attr <- list(...)
	res
}

plot.Window3d <- function(obj) {
	require(rgl)
	do.call("open3d",obj$attr)
	Chainable.Scene()
}

# just an object to offer chainable scene
Chainable.Scene <- function() {
	chain <- list()
	class(chain)<-"Chainable.Scene"
	return(invisible(chain))
}

"%<<%.Chainable.Scene" <- function(parent,obj) {
	plot(obj)
	Chainable.Scene()
}
