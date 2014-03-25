require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

del2Term$exprs <- list(substitute(2*l))

print(del2Term)

del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

del2Term$graph <- del2$graph

del2Term$infos <- c("x","l","l2")

del2Term$params <- list(theta=2)

del2Term$mode <- 0

del2Term$exprs<-list(substitute(theta*l))

del2Term$set_point( c(.5,.5) )

print(del2Term$eval_first_expr())



# new(Del2TermType3D) -> del3Term

# del3Term$exprs <- list(substitute(2*l))