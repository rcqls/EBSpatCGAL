require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

del2Term$exprs <- list(substitute(2*l))

print(del2Term)

del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

del2Term$graph <- del2$graph

del2Term$set_point( c(.5,.5))



# new(Del2TermType3D) -> del3Term

# del3Term$exprs <- list(substitute(2*l))