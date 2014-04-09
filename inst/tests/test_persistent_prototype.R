require(EBSpatCGAL)



d2 <- new.env()
d2$.rcpp <- new(Delaunay2)

PersistentRcppObject(d2, {
	print("coucou")
	obj$.rcpp <-new(Delaunay2)
}, 
{
	print("bye bye")
}
)