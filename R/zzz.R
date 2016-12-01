.onLoad <- function(libname, pkgname) {
	loadModule("cgal_module",TRUE)
	loadModule("tools_module",TRUE)
	loadModule("delaunay_module",TRUE)
}
