require(EBSpatCGAL)

new(Del2TermType3D) -> del2Term3D

del2Term3D$exprs <- list(substitute(2*l))

print(del2Term3D)

del3D <- Delaunay(3)

insert(del3D,matrix(runif(300),ncol=3))

del2Term3D$graph <- del3D$graph

del2Term3D$infos <- c("x","l","l2")

del2Term3D$params <- list(theta=2)

del2Term3D$mode <- 0

del2Term3D$exprs<-list(substitute(theta*l))

del2Term3D$set_point( c(.5,.5,.5))

print(del2Term3D$eval_first_expr())

# new(Del2TermType3D) -> del3Term

# del3Term$exprs <- list(substitute(2*l))