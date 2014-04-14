insert <- function(obj,...) UseMethod("insert")
"%<<%" <- function(obj,...) UseMethod("%<<%")
vertices <- function(obj,...) UseMethod("points")
edges <- function(obj,...) UseMethod("lines")
facets <- function(obj,...) UseMethod("facets")
cells <- function(obj,...) UseMethod("cells")


newEnv <- function (...) 
{
    args.call <- as.list(match.call())[-1]
    names.call <- names(args.call)
    if (is.null(names.call)) 
        names.call <- rep("", length(args.call))
    class <- as.character(args.call[nchar(names.call) == 0])
    names.call <- names.call[nchar(names.call) > 0]
    obj <- new.env()
    for (nm in names.call) assign(nm, 
        eval.parent(args.call[[nm]]),envir=obj)
    class(obj) <- class
    obj
}