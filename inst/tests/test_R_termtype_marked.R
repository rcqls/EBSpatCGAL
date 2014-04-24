require(EBSpatCGAL)
del2 <- Delaunay()
n <- 100
insert(del2,x=runif(n,-350,350),y=runif(n,-350,350),m=runif(n))
interMngr <- InteractionMngr(del2 ~ 10 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80)*(v[[1]]$m),th=c(2,4))|m ~ unif(0,1))
interMngr$terms[[1]] -> tmp

print(vertices(del2,"all"))

del2$rcpp()$update_infinite_vertex_info(list(m=NA))

tmp[del2]