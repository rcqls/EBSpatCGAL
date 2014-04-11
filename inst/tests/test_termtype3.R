require(EBSpatCGAL)

new(Del2TermType3D) -> del2Term3D

print(del2Term3D)

del3D <- Delaunay(3)

insert(del3D,matrix(runif(3000),ncol=3))

del2Term3D$set_graph(del3D$rcpp())

del2Term3D$infos <- c("x","l","l2")

del2Term3D$params <- list(theta=2)

del2Term3D$mode <- 0

del2Term3D$exprs<-list(a2=substitute(theta2*l),a1=substitute(theta*c(l,sqrt(l2))))
del2Term3D$exprs.size <- c(1,2)

del2Term3D$set_point( c(.5,.5,.5))

print(del2Term3D$eval_first_expr())

print(del2Term3D$eval_exprs())

insert(del3D,matrix(c(.5,.5,.5),ncol=3))

del2Term3D$set_index( 1001 )

print(del2Term3D$eval_first_expr())

print(del2Term3D$eval_exprs())