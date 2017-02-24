#' @export
column <- function(src, title, width=6, align="r", formatter=identity, parensrc=NULL){
  if(missing(title)) {
    title <- src
  }
  
  self <- new.env()
  
  self$src   <- src
  self$title <- title
  self$width <- width
  self$align <- align
  self$formatter <- formatter
  self$parensrc  <- parensrc
  
  self$span <- if(is.null(self$parensrc)) { 1 } else { 2 }
  class(self) <- "gotable.column"

  self$gettitle <- function() {
    format(self$title, width=self$width, justify="centre")
  }
  
  self$get <- function(data,i) {
    data[ i, self$src ]
  }
  
  self$getparen <- function(data,i) {
    data[ i, self$parensrc]
  }
  
  self$format <- function(data,i) {
    nrv <- lapply(FUN=function(d) format(formatter(d), width=self$width), self$get(data,i) )
    if(length(nrv) > 0) {
      nrv <- nrv %>% unlist() %>% as.matrix()
      if(!is.null(parensrc)) {
        pnrv <- lapply(FUN=function(d) format("(" %:% formatter(d) %:% ")", width=self$width), self$getparen(data,i) )
        nrv <- cbind(nrv, pnrv)
      }
      nrv
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