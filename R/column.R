column <- function(datacolumn, name, align="r" ){
  if(missing(name)) {
    name <- datacolumn
  }
  
  nrv <- list(  src       = datacolumn, 
                name      = name, 
                width     = 1, 
                align     = align )
  class(nrv) <- "gotable.column"
  nrv
}

variable <- function( column ) {
  self     <- list( column = column$src )
  self$get <- function(data,i) {
    data[ i, self$column ]
  }
  
  self$format <- function(data,i) {
    nrv <- lapply(FUN=as.character, self$get(data,i) )
    if(length(nrv) > 0) {
      nrv %>% unlist() %>% as.matrix()
    } else {
      NULL
    }
  }
  
  class(self) <- "gotable.variable"
  self
}

addtable.gotable.column <- function(def, table) {
  nrv <- gotable.clone( table )
  nrv$addcolumn( def )
  nrv
}