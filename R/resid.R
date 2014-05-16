Resid <- function(model,...,runs=10000,domain=Domain(c(-350,-350),c(350,350)),forms) {
	# almost everything is made in GNZCache
	self <-  GNZCache(model,...,runs=runs,domain=domain,forms=forms)
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