require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(500,-350,350),ncol=2))

#lc <- GNZCache(list(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)) , substitute(del2(l<=20)), substitute(del2(20<l & l<=80))))
gnz <- GNZCache(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)),1,del2(l<=20), del2(20<l & l<=80),
			runs=10000L,
			domain=Domain(c(-300,-300),c(300,300))
		)


#print(
#get.GNZCache(lc)
#) # result in lc$cexprs.cachelist

run(gnz,Th=c(0,0),Th2=0,single=-log(1/2500))