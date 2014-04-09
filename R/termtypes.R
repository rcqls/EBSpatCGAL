############################################### TermTypes
## Declaration of TermTypes here
## This function initialize .TermTypes in global environment

.TermTypesInit <- function() {
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

##########################################
#        Interaction Manager 
########################################## 

InteractionMngr <- function(formula,mode="default") {
  interMngr <- new.env()
  interMngr$formula <- formula
  interMngr$termtypes <- terms(formula)
  if(length(attr(interMngr$termtypes,"response"))) {
    interMngr$response<-interMngr$termtypes[[1]] #register only the response as a R call!!
    interMngr$termtypes<-interMngr$termtypes[-1]
  }
  class(interMngr) <- InteractionMngr
  interMngr
}

terms.InteractionMngr <- parseTermTypes<-function(e) {
  if(length(e)>1) {
    if(e[[1]]==as.name("+")) return(unlist(c(parseTermTypes(e[[2]]),parseTermTypes(e[[3]]))))
    if(e[[1]]==as.name("~")) {
      if(length(e)==3) {
        res <- unlist(c(e[[2]],parseTermTypes(e[[3]]))) 
        attr(res,"response") <- TRUE
      } else res <- unlist(c(parseTermTypes(e[[2]])))  
      return(res) 
    }
    if(e[[1]]==as.name("(")) return(parseTermTypes(e[[2]]))
    if(inherits(e[[1]],"name") && (type<-as.character(e[[1]])) %in% names(.TermTypes$convertTermType)) {
      e[[1]]<-as.name(paste("TermType",sep=""))
      ee<-as.list(e)
      e<-as.call(c(ee[1],.TermTypes$id[[ .TermTypes$convertTermType[[type]] ]],ee[-1]))
    }
    return(e)
  } else {
    if(is.name(e)) {
      e2 <- eval(e)
      ## TODO: Gibbs has to have a formula method
      if(inherits(e2,"Gibbs")) return(parseTermTypes(formula(e2)))
      else if(inherits(e2,"TermType")) return(parseTermTypes(formula(e2)))
      else return(e)
    }
    if(is.numeric(e)) return(e)
    if(inherits(e,"formula")) return(parseTermTypes(e))
    if(e==as.name("+")) return(c()) else return(e)
  }
}

##########################################
#       TermType Manager 
########################################## 
## TODO: MARKS and varsEnvir to connect to this stuff!!!!

TermTypeMngr <- function(type,callR,mode="default") {
  # auto initialize .TermTypes global variable!
  if(!exists(".TermTypes",envir=globalenv())) .TermTypesInit() 

  # the TermType manager
  termMngr <- new.env()
  termMngr$mode <- mode
  termMngr$type <- type
  termMngr$id <- .TermTypes$id[[type]]
  termMngr$callR <- callR

  class(termMngr) <- "TermTypeMngr"
  parse.TermTypeMngr(termMngr)
  termMngr
}

# build the result
parse.TermTypeMngr<-function(termMngr,skip=2) {
  
  ####### local functions
  # list has names? 
  is.named<-function(l) {
    nl<-names(l)
    if(length(l)>0 & length(nl)==0) nl<-rep("",length(l))
    sapply(nl,nchar)>0
  }
  # name objects inside expression
  find.names.in.expression<-function(e) {
    if(length(e)>1) return(as.vector(unlist(sapply(2:length(e),function(i) find.names.in.expression(e[[i]])))))
    if(inherits(e,"name")) return(as.character(e))
  }
  ##########################

  # The real starting point is here!
  # Extract the TermType components and the arguments (graph and components) 
  comps<- as.list(termMngr$callR)[-(1:skip)]
  print(comps)
  #form first containing the unnamed values of comps 
  form<-comps[!is.named(comps)]
  opts<-list() #for additional
  #components
  comps<-comps[ is.named(comps) ]
  #cat("AV:comps->");print(comps)
  comps2<-comps[ !(names(comps) %in% c("size",.TermTypes$args[[termMngr$type]])) ] #it is a list!
  #cat("AV:comps2->");print(comps2)
  size<-eval(comps[["size"]])
  if(is.null(size)) size<-list()
  #preliminary conversion depending on the mode
  comps1<-list() #automatic named values


  # parse the original expression depending on the mode
  parseTermTypeMode<-paste("parseTermTypeMode",termMngr$mode,sep=".")
  if(!exists(parseTermTypeMode) || !is.function(eval(parse(text=parseTermTypeMode)))) parseTermTypeMode <- "parseTermTypeMode.default"
  term<-do.call(parseTermTypeMode,list(form=form)) #return list(form=...,comps=...,size=..)
   
  #update term fields!
  form<-term$form
  if(length(term$comps)>0) comps2<-c(term$comps,comps2)  
  if(length(term$size)>0) size<-c(size,term$size)
  ## debugMode: cat("comps2->");print(comps2);print(size)
  
  #update the components size (default is 1!)
  sizeComp<-rep(1,length(comps2)) #it is a vector
  names(sizeComp)<-names(comps2)
  if(!is.null(size) && all(is.named(size))) 
    for(nm in names(size)) {
      sizeComp[[nm]]<-size[[nm]]
    }

  #further
  args<-list()
  if(any(.TermTypes$args[[termMngr$type]] %in% names(comps))) {
    args<-comps[ .TermTypes$args[[termMngr$type]] ] #it is a list!
    args<-args[!is.na(names(args))] #some names were NA values!
  }

  #infos and varsList
  namesList<-lapply(comps2,function(c) find.names.in.expression(c))
  unique(unlist(namesList,use.names=FALSE))->infos
  ### debugMode: cat("namesList->");print(namesList);print(infos)
  if(!is.null(form)) infos<-unique(c(infos,find.names.in.expression(form)))
  infos<-intersect(infos,.TermTypes$infos[[termMngr$type]]) 
  ### debugMode: cat("infos(after intersect)->");print(infos)
  ### => infos are definitely determined! ##cat("infos ->");print(infos)
  
  #named formulas => not an info no more marks
  varsList<-sapply(namesList,function(vars) setdiff(vars,c(infos))) #TODO MARKS! ,.funcEnv$.marks.names)))
  ### debugMode: cat("varsList->");print(varsList);print(infos)
  isFunc<-sapply(varsList,function(vars) length(vars)>0)
  ### => isFunc definitely determined! ##cat("isFunc ->");print(isFunc)
  varsList<-unique(unlist(varsList,use.names=FALSE)) 
  if(!is.null(form)) varsList<-unique(c(varsList,setdiff(find.names.in.expression(form),infos))) 
  ### => varsList definitely determined! ##cat("varsList ->");print(varsList)
  varsList<-setdiff(varsList,sapply(comps2,function(e) names(comps2)[!is.null(find.names.in.expression(e))]))
  
  # TODO: MARKS! varsList<-setdiff(varsList,.funcEnv$.marks.names)
  ### cat("varsList (last) ->");print(varsList)
  
  isVar<- names(comps2) %in% varsList 
   
  if(length(comps2)==0) { #no component, only a formula!
    compFunc<-list()
    compFunc.size<-integer(0)
    comps<-list()
    comps.size<-integer(0)
    varsEnv<-new.env()
  } else {
    ###cat("comps2!!");print(comps2);print(isFunc)
    compFunc<-comps2[isFunc]
    ###print(compFunc)
    compFunc.size<-as.integer(sizeComp[isFunc])
    names(compFunc.size)<-names(compFunc)
    vars<-comps2[isVar & !isFunc]
    varsEnv<-new.env()
    #cat("assign vars->");print(vars)
    for(e in names(vars)) assign(e,eval(vars[[e]]),env=varsEnv) 
    #cat("varsEnv->"); for(e in names(vars)) {cat("$",e,"\n",sep="");print(get(e,env=varsEnv))}
    # RMK: eval used since vars[[e]] can be of class call (ex: th=c(2,4) as argument)!
    #cat("comps2 TOTO");print(comps2)
    #print(isVar & !isFunc)
    #cat("vars->");print(vars)
    comps<-comps2[!isVar & !isFunc]
    #print(comps)
    #print(!isVar & !isFunc)
    #print(sizeComp[!isVar & !isFunc])
    comps.size<-as.integer(sizeComp[!isVar & !isFunc])
    #print(comps.size)
    names(comps.size)<-names(comps)
  }
  #parse variable names: local or global? and maybe size!
  compsLoc<-list();compsLoc.size<-c()
  compsGlob<-list();compsGlob.size<-c()
  iL<-0;iG<-0
  for( i in seq(comps) ) {
    v<-unlist(strsplit(names(comps)[i],"\\.",perl=TRUE))
    if(v[1]=="") {v<-v[-1];v[1]<-paste(".",v[1],sep="")} # to correct names starting with "."
    sizeTmp<-NULL
    if( any(sizeMask<-(v[-1] %in% as.character(1:20))) ) {
      sizeTmp<-as.integer(v[-1][sizeMask][1])
    }
    choice<-which(c("g","b","l") %in% v[-1])
    if(length(choice)==0) choice<-"b" else choice<-c("g","b","l")[choice[1]]
    if(choice %in% c("g","b")) {
      iG<-iG+1
      compsGlob[iG]<-comps[i]
      names(compsGlob)[iG]<-v[1]
      compsGlob.size[iG]<-if(is.null(sizeTmp)) comps.size[i] else sizeTmp
      names(compsGlob.size)[iG]<-v[1]
    }
    if(choice %in% c("l","b")) {
      iL<-iL+1
      compsLoc[iL]<-comps[i]
      names(compsLoc)[iL]<-v[1]
      compsLoc.size[iL]<-if(is.null(sizeTmp)) comps.size[i] else sizeTmp
      names(compsLoc.size)[iL]<-v[1]
    }
  }

  compFuncLoc<-list();compFuncLoc.size<-c()
  compFuncGlob<-list();compFuncGlob.size<-c()
  iL<-0;iG<-0
  for( i in  seq(compFunc) ) {
    v<-unlist(strsplit(names(compFunc)[i],"\\.",perl=TRUE))
    if(v[1]=="") {v<-v[-1];v[1]<-paste(".",v[1],sep="")} # to correct names starting with "."
    sizeTmp<-NULL
    if( any(sizeMask<-(v[-1] %in% as.character(1:20))) ) {
      sizeTmp<-as.integer(v[-1][sizeMask][1])
    }
    choice<-which(c("g","b","l") %in% v[-1])
    if(length(choice)==0) choice<-"b" else choice<-c("g","b","l")[choice[1]]
    if(choice %in% c("g","b")) {
      iG<-iG+1
      compFuncGlob[iG]<-compFunc[i]
      names(compFuncGlob)[iG]<-v[1]
      compFuncGlob.size[iG]<-if(is.null(sizeTmp)) compFunc.size[i] else sizeTmp
      names(compFuncGlob.size)[iG]<-v[1]
    }
    if(choice %in% c("l","b")) {
      iL<-iL+1
      compFuncLoc[iL]<-compFunc[i]
      names(compFuncLoc)[iL]<-v[1]
      compFuncLoc.size[iL]<-if(is.null(sizeTmp)) compFunc.size[i] else sizeTmp
      names(compFuncLoc.size)[iL]<-v[1]
    }
  }
  
  termMngr$form<-paste(deparse(form),collapse="") #the first string without name is the formula
  termMngr$vars<-varsEnv #envir (for dynamic trick) containing variables which are numeric named parameters (see param.EBGibbs and run.EBGibbs for the use),
  termMngr$varsList<-varsList #used in param.EBFunc: a priori does not match with ls(env=varsEnv) since maybe some variables not yet initialized or some variables in varsEnv are useless!
  termMngr$local <- list(
    cexprs=list(term=compsLoc,text=sapply(compsLoc,deparse),size=compsLoc.size),
    exprs=list(term=compFuncLoc,text=sapply(compFuncLoc,deparse),size=compFuncLoc.size)
  )
  termMngr$global <- list(
    cexprs=list(term=compsGlob,text=sapply(compsGlob,deparse),size=compsGlob.size),
    exprs=list(term=compFuncGlob,text=sapply(compFuncGlob,deparse),size=compFuncGlob.size)
  )
  termMngr$args<-args #arguments of the interaction function (ex: order=2)
  termMngr$opts<-opts #additional arguments (ex: nbParam=3 providing the number of param)
  termMngr$infos<-infos #list of the names of the infos

}

parseTermTypeMode.default <- function(form) {

  term<-list()

  if(length(form)>0) {
  ##if(length(form)>1) {
    term$comps<-form ##[-1] and ".V" introduced below! 
    if(length(term$comps)>1)  names(term$comps)<-c(".V",paste(".f",1:(length(term$comps)-1),sep="")) else names(term$comps) <- ".V"
    term$form<-form[[1]]
  ##} else tmp$form<-form[[1]]
  } else term$form<-NULL

  term
}

parseTermTypeMode.Pseudo <- function(form) {
  #   tmp<-list()
  #   switch(opts$mode,
  #   P=,Pseudo={
  #     #TODO: testing whether length(form) > 1 
  #     tmp$nbParam<-length(form)-1
  #     tmp$comps<-form
  #     names(tmp$comps)<-c(".V",paste(".dV",1:tmp$nbParam,sep=""))
  #     tmp$form<-NULL
  #   },
}

parseTermTypeMode.PseudoExpo <- function(form) {
  #   PE=,PseudoExpo={
  #     tmp$nbParam<-length(form)
  #     tmp$comps<-form
  #     tmp$comps<-list(.vc=as.call(c(as.name("c"),tmp$comps)),.vf=as.list(parse(text=".vc"))[[1]])
  #     #names(tmp$comps)<-".vc"
  #     tmp$size<-list(.vc=tmp$nbParam,.vf=tmp$nbParam)
  #     tmp$form<- as.call(c(as.name("sum"),as.call(c(as.name("*"),as.name("par"),tmp$comps$.vc))))
  #   },
}


testParseTermTypeExpression<-function(id,...) {
  callR<-match.call()
  parseTermTypeExpression(id,callR)
}