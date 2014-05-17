require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200,-350,350),ncol=2))

resid <- Resid(
				del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)) ,
				1,del2(l<=20), del2(20<l & l<=80),
				all2(range=100|l<=20),
				all2(range=100|20<l & l<80),
				del3(ta),
			runs=10000L,
			domain=Domain(c(-250,-250),c(250,250))
		)

run(resid,Th=c(2,4),Single=2)