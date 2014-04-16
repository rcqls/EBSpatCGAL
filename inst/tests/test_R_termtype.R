require(EBSpatCGAL)


cat("tmp!\n")

TermType(2,th*(l<30),toto=(l>40)*th,th=2) -> tmp

print(tmp$mngr$local)

cat("tmp2!\n")
TermType(2,th*(l<30),th=2) -> tmp2

print(tmp2$mngr$local)


cat("tmp3 like tmp2!\n")

interMngr <- InteractionMngr(~Del2(th*(l<30),th=2))

interMngr$terms[[1]] -> tmp3

print(tmp3$mngr$local)