require(EBSpatCGAL)

del2 <- Delaunay(2)

insert(del2,matrix(runif(200),ncol=2))

print(del2$graph$conflicted_and_boundary_edges( c(.5,.5)))