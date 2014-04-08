############################################### TermTypes
## Declaration of TermTypes here

TermTypes <- function() {
  TermTypes <- list(
                id=list(
                      ##clique type (i<i+1 allows us to easily insert new id!)
                      Del1=(i<-0),Del2=(i<-i+1),Del3=(i<-i+1),All2=(i<-i+1),NNG=(i<-i+1)
                    )
                )

  TermTypes$type<-names(TermTypes$id)

  TermTypes$convertTermType <- list(
    All2="All2",A2="All2",ALL2="All2",a2="All2",all2="All2",
    Del1="Del1",D1="Del1",DEL1="Del1",d1="Del1",del1="Del1",
    Del2="Del2",D2="Del2",DEL2="Del2",d2="Del2",del2="Del2",
    Del3="Del3",D3="Del3",DEL3="Del3",d3="Del3",del3="Del3",
    NNG="NNG",NN="NNG",Nn="NNG",nn="NNG"
  )

  TermTypes$infos<-list(
    Del1=c("id","x","v","a"),
    Del2=c("id","x","v","a","l2","l","ol2","ol","da"),
    Del3=c("id","x","v","a","ta","tp","c","r2","r","sa","ga"),
    All2=c("id","x","v","l2","l"),
    NNG=c("id","x","v","l2","l")
  )

  ## additonal names for declaration of interaction
  TermTypes$args<-list(
  Del2="order",
  All2="range",
  NNG="order"
)

  TermTypes$envir <- new.env()

  ## to determine the size of carac and compFunc!
  TermTypes$infosTest<- function(type,struct=NULL) {
    termEnv <- .TermTypes$envir
    #ok <- (!is.null(struct) && inherits(struct,"EBVor") && is.marked(struct))
    ok <- FALSE
    switch(type,
      Del1={termEnv$id<-1L;termEnv$x<-c(0,0);if(ok) termEnv$v <- eval(parse(text=struct$del.marks.gen));termEnv$a<-0},
      Del2={termEnv$id <- c(1L,2L);termEnv$x<-list(c(0,0),c(1,1));if(ok) termEnv$v<-lapply(1:2,function(i) eval(parse(text=struct$del.marks.gen)));termEnv$a<-c(0,0);termEnv$l2<-0;termEnv$l<-0;termEnv$ol2<-0;termEnv$ol<-0;termEnv$da<-0},
      Del3={termEnv$id<-c(1L,2L,3L);termEnv$x<-list(c(0,0),c(0,1),c(1,0));if(ok) termEnv$v<-lapply(1:3,function(i) eval(parse(text=struct$del.marks.gen)));termEnv$a<-c(0,0,0);termEnv$ta<-0;termEnv$tp<-0;termEnv$c<-c(0,0);termEnv$r2<-0;termEnv$r<-0;termEnv$sa<-0;termEnv$ga<-0},
      All2={termEnv$id<-c(1L,2L);termEnv$x<-list(c(0,0),c(1,1));if(ok) termEnv$v<-lapply(1:2,function(i) eval(parse(text=struct$del.marks.gen)));termEnv$l2<-0;termEnv$l<-0},
      NNG={termEnv$id<-c(1L,2L);termEnv$x<-list(c(0,0),c(1,1));if(ok) termEnv$v<-lapply(1:2,function(i) eval(parse(text=struct$del.marks.gen)));termEnv$l2<-0;termEnv$l<-0}
    )
    return(invisible())
  }

  TermTypes$length<- function(expr) return(length(eval(expr,envir=.TermTypes$envir)))
  # export in globalenv as .TermTypes
  assign(".TermTypes",TermTypes,envir=globalenv())
}

###################################################
##    Component Functional Formulae Manager      ##
###################################################

ComponentFunctionalFormulaManager <- function() {
  # auto initialize
  if(!exists(".TermTypes",envir=globalenv())) TermTypes()
  formMngr <- new.env()
  formMngr$formulas <- list()
  formMngr$origFormulas <- list()
  formMngr$compFuncList <- list()
  formMngr$compFuncCpt <- 0
  ## init carac environment for autoCaracFormula use!
  formMngr$caracEnv <- list()
  class(formMngr) <- "ComponentFunctionalFormulaManager"
  formMngr
}

## convert a form to a form with its related TermType object
## Rmk: useful for EBResid objects!
## Call: formula(formMngr,formula,)

## 2) formula(formMngr) prints the result

formula.ComponentFunctionalFormulaManager <- function(formMngr,form=NULL,struct=NULL,local=NULL) {
  ## convert code in substitute(code) (.e. a call) except if code is already a call!
  form.is.call <- try(is.call(form),TRUE)
  if(inherits(form.is.call,"try-error") || !form.is.call) form <- substitute(form)

  if(is.null(form)) {
    ## return the formula

    return(list(formulas=formMngr$formulas,func=formMngr$func))

  } else { 

    compFuncTypes <- names(.TermTypes$convertTermType)

    formMngr$origFormulas[[length(formMngr$origFormulas)+1]] <- form


    get.caracEnv <- function(type) { #type="compFunc"
      if(is.null(formMngr$caracEnv[[type]])) { 
        formMngr$caracEnv[[type]] <<- new.env()
        formMngr$caracEnv[[type]]$List <<- list()
        formMngr$caracEnv[[type]]$Cpt <<- 0
      } 
      return(formMngr$caracEnv[[type]])
    }

    convertCompFunc<-function(e) {
      if(length(e)>1) {
        if(as.character(e[[1]])[1] %in% compFuncTypes ) {
          key <- paste(substring(e[[1]],1,1),tolower(substring(e[[1]],nchar(e[[1]]))),sep="")
          e[[1]] <- as.name(key)
          formMngr$compFuncCpt <- formMngr$compFuncCpt + 1
          attr(e,"name") <- paste(".f",formMngr$compFuncCpt,sep="")
          tmp <- convertTermType(e)
          if(is.null(formMngr$compFuncList[[tmp$key]])) formMngr$compFuncList[[tmp$key]] <<- tmp$call 
          else formMngr$compFuncList[[tmp$key]] <<- as.call(unlist(c(as.list(formMngr$compFuncList[[tmp$key]]),tmp$call)))
          ##cat(tmp$key,"->");print(compFuncList[[tmp$key]])
          return(as.name(attr(e,"name")))
        }
        return(as.call(lapply(e,convertCompFunc)))
      } else return(e) 
    }

    convertTermType <- function(e) {
      ## print("convertTermType");print(e)
      funcName <- attr(e,"name")
      key <- as.character(e[[1]])
      if(is.null(local)) local <- tolower(substr(key,1,1)->tmp)==tmp
      key <- .TermTypes$convertTermType[[key]] #switch(toupper(key),A2="All2",D1="Del1",D2="Del2",D3="Del3",NN="NNG")
      opt <- .TermTypes$args[[key]]  #switch(key,All2="range",Del2=,Del3=,NNG="order",NULL)
      
      funcName2 <- paste(funcName,if(local) "l" else "g" ,sep=".")

      ## if we need to automatically determine the length! 
      .TermTypes$infosTest(key,struct) ## struct only required for marks length!

      e2 <- list(as.name(key))
      optVal <- ""
      if(length(e[[2]])>1 && e[[2]][[1]]=="|" && length(e[[2]])==3) {
        if(is.null(opt)) stop("In compFunc formula, type ",key," has no optional argument!")
        optVal <- e2[[2]] <- e[[2]][[2]] #value of opt
        names(e2)[2] <- opt #name of opt
        e[[2]] <- e[[2]][[3]]      
      }
      key2 <- if(nchar(optVal)) paste(key,".",optVal,sep="") else key 
      ## expression
      compFuncForm <- autoCaracFormula(e[[2]],key,local,get.caracEnv(key2),TRUE)
      ## print(compFuncForm)
      e2[[(length(e2)+1)->ii]] <- compFuncForm$form
      if(length(e)==3) {
        funcName2 <- paste(funcName2,e[[3]],sep=".")
        e <- e[-3]
      } else if(!inherits(tmp <- try(.TermTypes$length(e[[2]]),TRUE),"try-error") && tmp>1) {
        funcName2 <- paste(funcName2,tmp,sep=".")
      }
      names(e2)[ii] <- funcName2
      ##e2[[(length(e2)+1)->ii]] <- as.name(funcName)
      ##names(e2)[ii] <- funcName2

      ## finalization
      res <- as.call(e2)
      if(!is.null(formMngr$compFuncList[[key2]])) {
        res <- as.list(res)[-1]
        if(nchar(optVal)) res <- as.list(res)[-1]
      }

      return(list(call=res,key=key2 ))
    }
    
    formMngr$formulas[[length(formMngr$formulas)+1]] <-  convertCompFunc(form)

    ## Final ebfunc formula!

    ## Merge the caracs just after the compFuncs 

    makeTermTypeFormula <- function(rest,term=NULL) { ## term=NULL correspond to initial rest
      if(length(rest)==0) return(if(length(term)==0) NULL else as.call(c(as.name("~"),term)))
      if(is.null(term)) {
        if(length(rest)>1) makeTermTypeFormula(rest[-(1:2)],as.call(c(as.name("+"),rest[[1]],rest[[2]])))
        else return(as.call(c(as.name("~"),rest[[1]])))
      }
      else makeTermTypeFormula(rest[-1],as.call(c(as.name("+"),term,rest[[1]])))
    }

    ## Merge the caracs just after the compFuncs 
    compFuncList <- list()
    for(type in names(formMngr$compFuncList)) {
      compFuncList[[type]] <- as.call(unlist(c(as.list(formMngr$compFuncList[[type]]),formMngr$caracEnv[[type]]$List)))
    }

    formMngr$func <- makeTermTypeFormula(compFuncList)

    return(invisible())
  }
}

## DO NOT REMOVE! THIS ONE IS USED IN ComponentFunctionalFormulaManager
autoCaracFormula <- function(form,type="Del2",local=NULL,carac=new.env(),autoLength=FALSE) {

  ##parameter is a call of length 1, a name and with uppercase first letter 
  ##constant is the same but with lowercase first letter
  ##carac does not contain parameter in its expression
  ##compFunc is anything else

  if(is.numeric(type)) type <- .TermTypes$type[[type]]  

  contains.parameter <- function(e) {
    if(length(e)>1) {
      return(any(unlist(sapply(seq(e)[-1],function(i) contains.parameter(e[[i]])))))
    } else if(substr(e,1,1) %in% LETTERS)  return(TRUE) else return(FALSE)
  }

  contains.info <- function(e) {
    if(length(e)>1) {
      return(any(unlist(sapply(seq(e)[-1],function(i) contains.info(e[[i]])))))
    } else if(as.character(e) %in% .TermTypes$infos[[type]])  return(TRUE) else return(FALSE)
  }

  simplified.expr <- function(e) {
    if(length(e)>1) {
      if(e[[1]]==">") {e[[1]] <- as.name('<'); tmp <- e[[2]]; e[[2]]<- e[[3]]; e[[3]] <- tmp}
      if(e[[1]]==">=") {e[[1]] <- as.name('<='); tmp <- e[[2]]; e[[2]]<- e[[3]]; e[[3]] <- tmp}
      ee <- list(e[[1]])
      for(i in seq(e)[-1]) ee <- c(ee,simplified.expr(e[[i]]))
      return(as.call(ee))
    } else return(e)
  }

  #print(simplified.expr(~exp(l<40)^th[1]+th[2]*(l2>2000)+th3*(2000 < l2)))

  ## starting by finding parameters names
  
  if(!exists("List",envir=carac)) carac$List <- list()
  if(!exists("Cpt",envir=carac)) carac$Cpt <- 0

  parseExpr <- function(e) {
    if(contains.parameter(e)) {
      if(length(e)>1) return(as.call(unlist(c(e[[1]],lapply(seq(e)[-1],function(i) parseExpr(e[[i]]))))))
      else return(e)
    } else if(contains.info(e)) {
              ow <- options(warn=-1) #disable warning because length of carac$List and e are not multiple but e is not considered of length 1.
              if(length(tmp <- carac$List[carac$List == e]) > 0L) {
                tmp <- strsplit(names(tmp)[1],"\\.")[[1]] #strsplit to extract only the name of the carac!
                caracName <- if(tmp[1]=="") paste(tmp[1:2],collapse=".") else tmp[1]  
                return(as.name(caracName)) 
              } else {
                carac$Cpt <- carac$Cpt + 1
                caracName <- caracName2 <- paste(".c",carac$Cpt,sep="")
                if(!is.null(local)) caracName2 <- paste(caracName2,ifelse(local,"l","g"),sep=".")
                if(autoLength && ((tmp <- .TermTypes$length(e))>1)) caracName2 <- paste(caracName2,tmp,sep=".")
                carac$List[[caracName2]] <- if(e[[1]]=="(") e[[-1]] else e #no need of parenthesis!
                return(as.name(caracName))
              }
              options(warn=ow) #recover initial warning system
    } else return(e) 
    
  }

  return(list(form=parseExpr(simplified.expr(form)),caracList=carac$List))

}

 