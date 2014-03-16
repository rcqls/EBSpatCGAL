require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

(sc <- Scene()) %<<% window2d(xlab="",ylab="",main="what a beautiful plot!") 
sc %<<% points(del2,col="blue")  %<<% lines(del2,type="vor") 
sc %<<% lines(del2)
sc %<<% lines(del2,pt=(pt<-c(.5,.5)),col="green")
#sc %<<% points(del2,pt=pt,col="violet")

plot(sc)
points(pt[1],pt[2])

print(del2$graph$conflicted_and_boundary_edges( pt)->edges)

segments(edges$boundary_edges[,1],edges$boundary_edges[,2],edges$boundary_edges[,3],edges$boundary_edges[,4],lty=3,lwd=3)
segments(edges$conflicted_edges[,1],edges$conflicted_edges[,2],edges$conflicted_edges[,3],edges$conflicted_edges[,4],lty=3,lwd=1)

del2$graph$conflicted_faces( c(.5,.5)) -> facesMoins

rbind(facesMoins[,1:4],facesMoins[,3:6],facesMoins[,c(1:2,5:6)]) -> edgesMoins
print(edgesMoins)