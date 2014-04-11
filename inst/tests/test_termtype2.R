require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

print(del2Term)

del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

del2Term$set_graph(del2$rcpp())

del2Term$infos <- c("x","l","l2")

del2Term$params <- list(theta=2,theta2=3)

del2Term$mode <- 0

del2Term$exprs<-list(a2=substitute(theta2*l),a1=substitute(theta*c(l,sqrt(l2))))
del2Term$exprs.size <- c(1,2)

del2Term$set_point( c(.5,.5) )

print(del2Term$eval_first_expr())

print(del2Term$eval_exprs())

insert(del2,matrix(c(.5,.5),ncol=2))

del2Term$set_index( 101 )

print(del2Term$eval_first_expr())

print(del2Term$eval_exprs())
