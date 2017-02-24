#' @export
group <- function(varname, values) {
  nrv <- list(varname=varname, values=values)
  class(nrv) <- "gotable.group"
  nrv
}

rowset <- function(subrows) {
  self <- new.env()
  
  self$subrows <- subrows
  
  self$format <- function(variables) {
    nrv <- list()
    for(level in names(self$subrows) ) {
      nrv[[level]] <- subtable( self$subrows[[level]]$format(variables), level )
    }
    nrv
  }
  
  self$split <- function(group) {
    rowset( lapply(FUN=function(s) s$split(group), self$subrows)  )
  }
  class(self) <- "gotable.rowset"
  self
}

print.gotable.rowset <- function(x,...) {
  for(s in x$subrows) {
    print( s )
  }
}

addtable.gotable.group <- function(def, table) {
  nrv <- gotable.clone( table )
  nrv
}