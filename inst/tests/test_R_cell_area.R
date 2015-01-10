require(EBSpatCGAL)
del2 <- Delaunay()
insert(del2,x=runif(n<-100),y=runif(n))
## area
#print(sapply(seq(del2),function(i) del2$rcpp()$cell_area(i)))
print(area(del2))

print(volume(del2,3))
