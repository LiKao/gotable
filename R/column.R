#' @export
column <- function(src, title, width=6, align="r" ){
  if(missing(title)) {
    title <- src
  }
  
  self <- new.env()
  
  self$src   <- src
  self$title <- title
  self$width <- width
  self$align <- align
  class(self) <- "gotable.column"

  self$get <- function(data,i) {
    data[ i, self$src ]
  }
  
  self$format <- function(data,i) {
    nrv <- lapply(FUN=function(d) format(d, width=self$width), self$get(data,i) )
    if(length(nrv) > 0) {
      nrv %>% unlist() %>% as.matrix()
    } else {
      NULL
    }
  }
  
  self
}

#' @export
addtable.gotable.column <- function(def, table) {
  nrv <- gotable.clone( table )
  nrv$addcolumn( def )
  nrv
}