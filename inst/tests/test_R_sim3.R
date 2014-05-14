require(EBSpatCGAL)

del3 <- Delaunay(3)

insert(del3,matrix(runif(300,-350,350),ncol=3))

# init gibbs 
#gd3 <- SimGibbs(del3 ~ 5 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(-2,10)),domain=c(-350,-350,-350,350,350,350)) 

#run(gd3)

# (sc3 <- Scene()) %<<% 
# window3d(c(-350,350),c(-350,350),c(-350,350)) %<<% 
# points(del3,col="red",radius=3) %<<% 
# (lines(del3,col="red",lwd=5) %with% (length <= 20))  %<<% 
# (lines(del3,lwd=5) %with% ( 20<length & length <= 80)) %<<% 
# (lines(del3,col="violet") %with% ( 80<length))

# plot(sc3)

(sc3 <- Scene(del=del3)) %<<% 
window3d(c(-350,350),c(-350,350),c(-350,350)) %<<% 
points(del,col="red",radius=3) %<<% 
lines(del,col="red",lwd=5,when= length <= 20)  %<<% 
lines(del,lwd=5,when=20<length & length <= 80) %<<% 
lines(del,col="violet",when=80<length)

plot(sc3)

del3bis <- Delaunay(3)

insert(del3bis,matrix(runif(30,-350,350),ncol=3))