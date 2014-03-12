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