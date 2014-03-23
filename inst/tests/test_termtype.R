require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

del2Term$exprs <- list(substitute(2*l))

print(del2Term)

new(Del2TermType3D) -> del3Term

del3Term$exprs <- list(substitute(2*l))