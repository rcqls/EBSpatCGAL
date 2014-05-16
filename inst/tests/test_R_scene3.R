require(EBSpatCGAL)

del3 <- Delaunay(3)

insert(del3,matrix(runif(300,-350,350),ncol=3))

(sc3 <- Scene(del=del3)) %<<% 
window3d(c(-350,350),c(-350,350),c(-350,350)) %<<% 
points(del,col="red",radius=3) %<<% 
lines(del,col="red",lwd=5,when= length <= 20)  %<<% 
lines(del,lwd=5,when=20<length & length <= 80) %<<% 
lines(del,col="violet",when=80<length)

plot(sc3)

del3bis <- Delaunay(3)

insert(del3bis,matrix(runif(30,-350,350),ncol=3))

plot(sc3,del=del3bis)