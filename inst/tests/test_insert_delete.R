require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

cat("Insersion of point c(.5,.5)\n")

del2$rcpp()$conflicted_faces( c(.5,.5)) -> facesMoins

rbind(facesMoins[,1:4],facesMoins[,3:6],facesMoins[,c(1:2,5:6)]) -> edgesMoins

insert(del2,c(.5,.5))

del2$rcpp()$incident_edges( del2$point.index ) -> edgesPlus
edgesPlus <- rbind(edgesPlus,cbind(edgesPlus[,1:2],edgesPlus[c(2:nrow(edgesPlus),1),1:2]))

print(edgesInsertion<-list(plus=edgesPlus,moins=edgesMoins))

cat("The reverse!\n")

edgesDeletion <- list(moins=edgesPlus)

del2$rcpp()$remove_at_pos(del2$point.index)

del2$rcpp()$conflicted_faces( c(.5,.5)) -> facesPlus

rbind(facesPlus[,1:4],facesPlus[,3:6],facesPlus[,c(1:2,5:6)]) -> edgesDeletion[["plus"]]

print(edgesDeletion)




