require(EBSpatCGAL)
del3 <- Delaunay(3)
insert(del3,x=runif(n<-100),y=runif(n),z=runif(n))
## volume
#print(sapply(seq(del3),function(i) del3$rcpp()$cell_volume(i)))


print(volume(del3))
