require(EBSpatCGAL)

del3 <- Delaunay(3)

insert(del3,matrix(runif(3000),ncol=3))

print(del3$graph$conflicted_and_boundary_edges( c(.5,.5,.5))->edgesMoins)

insert(del3,c(.5,.5,.5))

del3$graph$incident_edges( del3$point.index) -> edgesPlus

cat("Check same boundary before and after insertion!\n")

unique(rbind(edgesMoins$boundary_edges[,1:3],edgesMoins$boundary_edges[,4:6])) -> moins

unique(edgesPlus[,c(4:6)])-> plus

moins[order(moins[,1]),] -> moins
plus[order(plus[,1]),] -> plus 
print(moins);print(plus)
print(all(plus - moins  == 0))



