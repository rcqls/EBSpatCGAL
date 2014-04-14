require(EBSpatCGAL)

del2 <- Delaunay(2)

insert(del2,matrix(runif(200),ncol=2))

cat("edgesMoins\n")
print(del2$rcpp()$conflicted_and_boundary_edges( c(.5,.5))->edgesMoins)

insert(del2,c(.5,.5))

del2$rcpp()$incident_edges( del2$point.index) -> edgesPlus

cat("edgesPlus\n")

print(edgesPlus)

cat("Check same boundary before and after insertion!\n")

unique(rbind(edgesMoins$boundary_edges[,1:2],edgesMoins$boundary_edges[,3:4])) -> moins

unique(rbind(edgesPlus[,c(1:2)],edgesPlus[,c(3:4)])) -> plus

plus <- plus[apply(plus,1,function(v) all(v!=c(.5,.5))),]

moins[order(moins[,1]),] -> moins
plus[order(plus[,1]),] -> plus 
print(moins);print(plus)
print(all(plus - moins  == 0))