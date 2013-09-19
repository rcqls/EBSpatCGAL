
require(EBSpatCGAL)
dt2 <- new(Delaunay2)
dt2$insert(runif(100),runif(100))
v2 <-dt2$vertices( )
e2 <-dt2$edges( )
de2 <- dt2$dual_edges( )

plot(v2,asp=1)
segments(e2[,1],e2[,2],e2[,3],e2[,4],col="red")
segments(de2[,1],de2[,2],de2[,3],de2[,4],col="blue")

