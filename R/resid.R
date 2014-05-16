Resid <- function(model,...,domain=Domain(c(-350,-350),c(350,350)),runs=NULL,grid=NULL,forms) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,...,domain=domain,runs=runs,grid=grid,forms=forms)
	class(self) <- c("Resid",class(self))
	self
}


# just restrict the use of formula method
formula.Resid <- function(self,form=NULL,...) {
	if(!is.list(form)) formula.GNZCache(self,form=form,...)
	else {
		stop("List not allowed!")
	}
}

run.Resid <- function(self,...) {
	run.GNZCache(self,...) -> tmp
	sapply(seq(tmp$first),function(i) tmp$first[i] - tmp$second[i])
}