insert <- function(obj,...) UseMethod("insert")
delete <- function(obj,...) UseMethod("delete")
"%<<%" <- function(obj,...) UseMethod("%<<%")
"%when%" <- function(obj,...) UseMethod("%when%")
elements <- function(obj,...) UseMethod("elements")

vertices <- function(obj,...) UseMethod("vertices")
vertices.owin <- spatstat:::vertices #compatibility with spatstat

edges <- function(obj,...) UseMethod("edges")
facets <- function(obj,...) UseMethod("facets")
cells <- function(obj,...) UseMethod("cells")

params <- function(obj,...) UseMethod("params")
run <- function(obj,...) UseMethod("run")

Single <-function(obj,...) UseMethod("Single")
"Single<-" <-function(obj,...) UseMethod("Single<-")

"%contains%" <- function(obj,...) UseMethod("%contains%")

newEnv <- function (...,class.as.character) 
{
    args.call <- as.list(match.call())[-1]
    names.call <- names(args.call)
    if (is.null(names.call)) 
        names.call <- rep("", length(args.call))
    class <- if(missing(class.as.character)) as.character(args.call[nchar(names.call) == 0]) else class.as.character
    names.call <- names.call[nchar(names.call) > 0]
    obj <- new.env()
    for (nm in names.call) assign(nm,eval.parent(args.call[[nm]]),envir=obj)
    class(obj) <- class
    obj
}

# just to extend unif to discrete support too
rUnif <- function(n,min=0,max=1,support.discrete) {
    if(!missing(support.discrete)) runifDisc(n,support.discrete=support.discrete)
    else stats:::runif(n,min,max)
}

# unif for discrete support
runifDisc <- function(n,...,support.discrete) {
    if(missing(support.discrete)) support.discrete <- c(...)
    sample(support.discrete,n,repl=TRUE)
}