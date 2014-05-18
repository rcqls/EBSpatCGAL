require(EBSpatCGAL)

# init del3
del3m <- Delaunay(3)

insert(del3m,x=runif(100,-350,350),y=runif(100,-350,350),z=runif(100,-350,350),m=runifDisc(100,1,2))


plot(del3m,col=m)