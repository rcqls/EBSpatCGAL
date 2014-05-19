require(EBSpatCGAL)

del3 <- Delaunay(3)

insert(del3,matrix(runif(300,-350,350),ncol=3))

# init gibbs 
gd3 <- SimGibbs(del3 ~ 14 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(-2,10)),domain=Domain(c(-350,-350,-350),c(350,350,350))) 

run(gd3)

(sc3 <- Scene()) %<<% 
window3d(gd3) %<<% 
points(gr,col="red",radius=3) %<<% 
lines(gr,col="red",lwd=5,when= length <= 20)  %<<% 
lines(gr,lwd=5,when=20<length & length <= 80) %<<% 
lines(gr,col="violet",when=80<length)

plot(sc3,gr=del3)