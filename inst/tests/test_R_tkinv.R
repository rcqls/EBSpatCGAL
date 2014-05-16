require(EBSpatCGAL)

# del2 <- Delaunay()

# insert(del2,matrix(runif(200,-350,350),ncol=2))


if(TRUE) {
	tk <- TKInverse(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)), 
		runs=10000L,
		domain=Domain(c(-300,-300),c(300,300))
	)

	run(tk,Single=2,Th=c(2,4))

} else {
	tk <- TKInverse(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)+Th[3]*(l>80)), 
		runs=10000L,
		domain=Domain(c(-300,-300),c(300,300))
	)

	run(tk,Single=2,Th=c(2,4,0),fixed=4)
}

# method with horizon
