require(EBSpatCGAL)

# init del2
del2 <- Delaunay()

#insert(del2,matrix(runif(200,-350,350),ncol=2))


# init gibbs 
gd <- SimGibbs(del2 ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4))) 

run(gd)

# init scene => TODO: atomatically created later
(sc <- Scene(del=del2)) %<<% window2d(c(-350,350),c(-350,350),xlab="",ylab="",main="what a beautiful plot!") 
sc %<<% points(del,col="blue")  %<<% lines(del,type="vor") 
sc %<<% (lines(del) %with% (length < 100)) %<<% (lines(del,col="green") %with% ( 100 <= length))

plot(sc)

# delete some part
# delete(del2,inside=c(-300,-300,300,300))

# plot(sc)

# run(gd)

# plot(sc)