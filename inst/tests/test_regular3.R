require(rgl)
require(EBSpatCGAL)
rt3 <- new(Regular3)
rt3$insert(runif(100),runif(100),runif(100),runif(100))
v3 <-rt3$vertices( )
e3 <-t(matrix(t(rt3$edges( )),nr=3))
f3 <-t(matrix(t(rt3$facets( )),nr=3))

open3d()
points3d(v3)
segments3d(e3,col="red")
spheres3d(v3,radius=.01)
triangles3d(f3)

