require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200,-350,350),ncol=2))

resid <- Resid(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)) ,1,del2(l<=20), del2(20<l & l<=80),
			runs=10000L,
			domain=Domain(c(-300,-300),c(300,300))
		)

formula(resid,"inverse")

run(resid,Th=c(2,4),Single=2)

tk <- TKInverse(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)), 
		runs=10000L,
		domain=Domain(c(-300,-300),c(300,300))
)

run(tk,Single=2,Th=c(2,4))

# same result
cat("TEST ->")
print(tk$optim.function(c(2,2,4)));print(sum(run(resid,Single=2,Th=c(2,4))^2))
print(tk$optim.function(c(2,2,4))==sum(run(resid,Single=2,Th=c(2,4))^2))
cat("\n")


