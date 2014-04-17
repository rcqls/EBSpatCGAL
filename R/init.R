insert <- function(obj,...) UseMethod("insert")
"%<<%" <- function(obj,...) UseMethod("%<<%")
vertices <- function(obj,...) UseMethod("vertices")
edges <- function(obj,...) UseMethod("edges")
facets <- function(obj,...) UseMethod("facets")
cells <- function(obj,...) UseMethod("cells")

params <- function(obj,...) UseMethod("params")
run <- function(obj,...) UseMethod("run")


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