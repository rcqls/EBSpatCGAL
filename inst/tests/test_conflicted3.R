require(EBSpatCGAL)

del3 <- Delaunay(3)

insert(del3,matrix(runif(300),ncol=3))

print(del3$graph$conflicted_edges_and_boundary_edges( c(0.5,.5,.5)))