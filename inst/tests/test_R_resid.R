require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200,-350,350),ncol=2))

#lc <- GNZCache(list(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)) , substitute(del2(l<=20)), substitute(del2(20<l & l<=80))))
resid <- Resid(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80))+Del1(Th2*(a<20)),1,del2(l<=20), del2(20<l & l<=80),
			runs=10000L,
			domain=c(-300,-300,300,300)
		)

run(resid,Th=c(0,0),Th2=3,single=-log(1/3600))