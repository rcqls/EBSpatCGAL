
require(EBSpatCGAL)
rt2 <- new(Regular2)
rt2$insert(runif(100),runif(100),runif(100))
v2 <-rt2$vertices( )
e2 <-rt2$edges( )
de2 <- rt2$dual_edges( )

plot(v2,asp=1)
segments(e2[,1],e2[,2],e2[,3],e2[,4],col="red")
segments(de2[,1],de2[,2],de2[,3],de2[,4],col="blue")