Scene <- function() {
	obj <- newEnv(Scene)
	obj$list <- list()
	obj
}

length.Scene <- function(obj) length(obj$list)

plot.Scene <- function(obj,subset) {
	l <- if(missing(subset)) obj$list else obj$list[subset]
	for(comp in l) if(!is.null(comp)) plot(comp)
}

"[[.Scene" <- function(obj,ref) obj$list[[ref]] #ref is integer or string

"[[<-.Scene" <- function(obj,ref,comp) {
 	obj$list[[ref]] <- comp #ref is integer or string
 	obj
}

# extract some art of the scene
"[.Scene" <- function(obj,ref) {
	obj2 <- Scene()
	if(missing(ref)) obj2$list <- obj$list
 	else obj2$list <- obj$list[ref]
 	obj2
}


## to insert components
"[<-.Scene" <- function(obj,ref,inc,comp) {
	if(missing(inc)) inc <- 0
	if(missing(comp)) comp <- inc
	tmp <- obj$list
	obj$list <- NULL
	if(is.character(ref)) ref <- which(ref %in% names(tmp))+inc
	if(ref>length(tmp)) ref <- length(tmp)+1
	if(ref==1) obj$list <- comp else obj$list <- c(tmp[1:(ref-1)],comp)
	if(ref<=length(tmp)) obj$list <- c(obj$list,tmp[ref:length(tmp)])
	obj
}

"%<<%.Scene" <- function(obj,comp) {
	#if(!missing(after))
	#if(!missing(before))
	obj$list[[length(obj$list)+1]] <- comp
	return(invisible(obj))
}


#update.Scene <- function(obj,formula) {#formula allow to change the order of the element
#
#}

window2d <- function(xrange=c(0,1),yrange=c(0,1),...) {
	res <- newEnv(Window2d)
	res$obj <- list(xrange,yrange)
	res$attr <- list(...)
	res
}

plot.Window2d <- function(obj) {
	do.call("plot",c(list(obj$obj[[1]],obj$obj[[2]],type="n",asp=1),obj$attr))
}

window3d <- function(xrange=c(0,1),yrange=c(0,1),zrange=c(0,1),...) {
	res <- newEnv(Window3d)
	res$obj <- list(xrange,yrange,zrange=c(0,1))
	res$attr <- list(...)
	res
}

plot.Window3d <- function(obj) {
	require(rgl)
	do.call("open3d",obj$attr)
}