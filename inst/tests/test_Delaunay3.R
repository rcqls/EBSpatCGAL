require(Rfirst)

del3 <- Delaunay(3)

insert(del3,matrix(runif(300),ncol=3))

(sc3 <- Scene()) %<<% window3d() 
sc3 %<<% points(del3,col="blue",radius=.01) 
sc3 %<<% lines(del3)  %<<% facets(del3,alpha=.05)
#sc %<<% lines(del3,type="vor")