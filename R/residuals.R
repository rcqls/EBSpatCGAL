## the idea: an object residObj of class Resid depending on a model and formulas

## run(residObj,vor,param,...) -> update caches with vor$pl

## if param is a character containing the estimation method name launch the estimation method 
## but the simplest solution is to provide the estimation as an argument of param.


## Ex: Resid(gd~Del2(a=l2)+All2(b=c2),del2(l<40),d2(l<60)) 

Resid <- function(model,...,nbPts=10000,domainSize,mode=c("random","systematic"),weighted=FALSE,compute=c("normal","noparam","inverse")) {
  if(!inherits(model,"formula")) warning("parameter model has to be a formula!")
  resid <- CqlsObj(Resid)
  resid$call <- match.call()
  resid$compute <- match.arg(compute)
  resid$formMngr <- CompFuncFormulaManager()

  ## build the ebfunc and formulas
  tmp <- as.list(substitute(c(...)))[-1] #the expressions of the formulas
  ## verboseMode: cat("tmp->");print(tmp)
  tmp <- strsplit(sapply(tmp,deparse),"\\|")
  resid$formulas <- list(model=model,fct=as.list(parse(text=paste("c(",paste(sapply(tmp,function(e) e[[1]]),collapse=","),")",sep="")))[[1]])

  ## verboseMode: cat("formulas ->\n");print(resid$formulas)

  tmp <- unlist(sapply(tmp,function(e) if(length(e)>1) e[[2]]))
  tmp <- if(is.null(tmp)) NULL else  as.formula(paste("~",paste(tmp,collapse="+")))
  


  formula(resid$formMngr,as.call(c(as.name("+"),as.name("Single"),(resid$formulas$model)[[3]])),local=TRUE)
  formula(resid$formMngr,resid$formulas$fct)
  
  ## print(formula(formMngr))
  resid$func <- Func(resid$formMngr$func,mode="default")

  resid$mode <- match.arg(mode)
  resid$weighted <- weighted

  resid$response <- eval(resid$formulas$model[[2]],parent.frame())
  if(missing(domainSize) && !is.null(resid$response)) domainSize <- resid$response$sizeIn
  if(length(domainSize)==1) domainSize <- c(domainSize,domainSize)
  resid$.domainSize <- domainSize
  if(weighted) {SumCache<- SumCacheCompFunc;SamplCache<-SamplCacheCompFunc}
  else {SumCache<-SumCache;SamplCache<-SamplCache}
  ## verboseMode: cat("sumCache->")
  ## poly may be NULL if response not initialized yet
  poly <- if(is.null(resid$response)) NULL else resid$response$pl
  resid$sumCache <- SumCache(resid$func,domainSize,poly) 
  ## verboseMode: cat("Done\n");cat("samplCache->")
  resid$samplCache <- SamplCache(resid$func,nbPts,domainSize,poly,mode) 
  resid$response_runs <- 0L
  resid
}

 
reactivate.Resid <- function(resid) { 
  reactivate(resid$func)
  reactivate(resid$sumCache)
  reactivate(resid$samplCache)
  reactivate(eval(resid$response,parent.frame()))
}


## do not forget to launch resid$response <- vor (i.e. Vor object) if not already provided in the formula call

update.Resid <- function(resid,verbose=TRUE,cacheOf=NULL) {
  if(verbose) cat("Please wait: updating object Resid ...")
  if(!is.null(resid$response)) { 
    resid$response <- eval(resid$formulas$model[[2]],parent.frame())
    resid$sumCache$poly <- resid$response$pl
    resid$samplCache$poly <- resid$response$pl
    update(resid$sumCache)
    update(resid$samplCache)
  }
  if(!is.null(cacheOf)) { #example: cacheOf=pseudo after a call to pseudo <- PseudoExpo(....)
    resid$sumCache <- cacheOf$sumCache
    resid$samplCache <- cacheOf$samplCache
    ## No Update or comparison!!!
  }
  .funcEnv$pas2 <- as.integer(sqrt(resid$samplCache$nbPts))^2
  resid$leftMat <- as.matrix(resid$samplCache)
  resid$rightMat <- as.matrix(resid$sumCache)
  prepare.formula.Resid(resid)
  if(verbose) cat(" -> Done!\n")
}

prepare.formula.Resid <- function(resid) {
  tmp <- resid$func$fct[[2]]$term$compFuncLoc
  tmp <- gsub("\\.c([[:digit:]]+)",".c[\\1]",tmp)
  tmp <- lapply(tmp,function(expr) as.list(parse(text=expr))[[1]])
  tmp2 <- resid$formMngr$formulas
  tmp2 <- lapply(tmp2,expression.replace,dict=tmp)
  resid$leftForm<-substitute(.form*exp(-(.V)),list(.V=tmp2[[1]],.form=tmp2[[2]]))
  resid$rightForm<-tmp2[[2]]
}

run.Resid<-function(resid,...) {
  leftForm <- update(resid$leftForm,dict=list(...))
  left <- apply(apply(resid$leftMat,1,function(.c) eval(leftForm)),1,sum)/.funcEnv$pas2
  if(resid$compute!="noparam" || !exists("right",envir=resid)) resid$right <- apply(apply(resid$rightMat,1,function(.c) eval(resid$rightForm)),1,sum)/resid$domainSize[1]/resid$domainSize[2]
  left-resid$right
}

## copy of run with paramList as a list
runWithParamList.Resid<-function(resid,paramList) {
  leftForm <- update(resid$leftForm,dict=paramList)
  left <- apply(apply(resid$leftMat,1,function(.c) eval(leftForm)),1,sum)/.funcEnv$pas2
  if(resid$compute!="noparam" || !exists("right",envir=resid)) resid$right <- apply(apply(resid$rightMat,1,function(.c) eval(resid$rightForm)),1,sum)/resid$domainSize[1]/resid$domainSize[2]
  left-resid$right
}


# residuals.Vor <- function(vor, resid, ...) {
#   if(is.null(resid$response) || !identical(resid$response,vor)) {
#     resid$response <- vor
#     resid$response_runs <- 0L #force the update!
#   }
#   run(resid, ...)
# }

