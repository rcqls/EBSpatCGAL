require(EBSpatCGAL)

# init del2
del2m <- Delaunay()

insert(del2m,x=runif(100,-350,350),y=runif(100,-350,350),m=runifDisc(100,1,2))


plot(del2m,col=m)