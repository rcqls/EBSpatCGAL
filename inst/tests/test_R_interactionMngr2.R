require(EBSpatCGAL)
del2 <- Delaunay()
insert(del2,x=runif(n<-100),y=runif(n))
interMngr <- InteractionMngr(del2 ~ Del1(a))