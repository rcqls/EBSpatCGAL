require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200,-350,350),ncol=2))


if(FALSE) {
	tk <- TKInverse(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)), 
		runs=10000L,
		domain=c(-300,-300,300,300)
	)
} else {
	tk <- TKInverse(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)+Th[3]*(l>80)), 
		runs=10000L,
		domain=c(-300,-300,300,300)
	)
}

# method with horizon
run(tk,single=-log(1/3600),Th=c(0,0,0),fixed=4)