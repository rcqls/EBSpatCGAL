require(EBSpatCGAL)


del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

dom <- new(DomainDel2D,del2$rcpp(),c(0,0),c(1,1)) 
