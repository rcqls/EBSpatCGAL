require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

new(Del2TermType2D) -> del2Term2


del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

del2Term$set_struct(del2$rcpp())
del2Term2$set_struct(del2$rcpp())

del2Term$infos <- c("x","l","l2")
del2Term2$infos <- c("x","l","l2")

del2Term$params <- list(theta=2,theta2=1)
del2Term2$params <- list(theta=2)

del2Term$exprs<-list(a2=substitute(theta2*l),a1=substitute(theta*c(l,sqrt(l2))))
del2Term$exprs.size <- c(1,2)
del2Term2$exprs<-list(a3=substitute(theta*l))
del2Term2$exprs.size <- c(1)

new(Interaction,list(del2Term,del2Term2)) -> inter

inter$set_current( c(.5,.5) )

print(inter$local_energy())

print(del2Term$eval_exprs())

del2Term2$set_current( c(.5,.5) )


print(del2Term2$eval_exprs())

# print(del2Term$eval_exprs())

# insert(del2,matrix(c(.5,.5),ncol=2))

# del2Term$set_index( 101 )

# print(del2Term$eval_first_expr())

# print(del2Term$eval_exprs())