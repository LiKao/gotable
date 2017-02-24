# Private version of nrow, that can also deal with slices of tables
.nrow <- function(d) {
  UseMethod(".nrow")
}

#' @export
.nrow.data.frame <- function(d) {
  nrow(d)
}

span <- function(r) {
  UseMethod("span")
}

subtable.row <- function(v) {
  nrv <- list(values=v)
  class(nrv) <- "gotable.subtable.row"
  nrv
}

#' @export
span.gotable.subtable.row <- function(r) {
  0
}

padding <- function(depth, format, width=7) {
  if(format=="latex") {
    delim <- format(" & ", width=width, justify="right")
  } else {
    delim <- paste(rep(" ", width), collapse="")
  }
  if(depth>0) {
    delim %:% padding(depth-1, format)
  } else {
    ""
  }
}

#' @export
as.character.gotable.subtable.row <- function(x, depth=0, format, ...) {
  if(format=="latex") {
    newl  <- " \\\\\n"
    delim <- " & "
  } else {
    newl  <- "\n"
    delim <- ""
  }
  padding(depth, format) %:% paste(x$values, collapse=delim) %:% newl
}

#' @export
print.gotable.subtable.row <- function(x, depth=0, ...) {
  cat( as.character(x, depth=depth ), ... )
}

subtable <- function(v, level) {
  nrv <- list(subrows=v, level=level)
  class(nrv) <- "gotable.subtable"
  nrv
}


#' @export
span.gotable.subtable <- function(r) {
  if(is.null(r$subrows)) {
    0
  } else {
    (lapply(FUN=span, r$subrows) %>% unlist() %>% max()) + 1
  }
}


#' @export
as.character.gotable.subtable <- function(x, depth=0, format, ...) {
  if(missing(format) || is.null(format)) {
    format = getOption("knitr.table.format")
  }
  if(is.null(format)) {
    format <- "plain"
  }
  
  if( is.null(x$subrows) ) {
    return("")
  }
  if( format == "latex" ) {
    newl  <- " \\\\\n"
    delim <- " & "
  } else {
    newl  <- "\n"
    delim <- ""
  }
  
  nrv       <- padding(depth, format=format)
  if( length(x$subrows) > 1 ) {
    sp <- span(x)
    if(format == "latex" && sp > 1 ) {
      nrv <- nrv %:% "\\multicolumn{" %:% sp %:% "}{l}{" %:% x$level %:% "}" %:% newl
    } else {
      nrv  <- nrv %:% format(x$level, width=7, justify="left") %:% newl
    }
    rows <- lapply( FUN=function(s) as.character(s, depth=depth+1, format=format, ...), x$subrows)
    nrv  <- nrv %:% paste(rows, collapse="")
  } else {
    nrv <- nrv %:% format(x$level, width=7-3, justify="left") %:% delim %:% as.character(x$subrows[[ 1 ]], format=format, ... )
  }
  nrv
}

#' @export
print.gotable.subtable <- function(x, depth=0, format, ... ) {
  if(missing(format) || is.null(format)) {
    format = getOption("knitr.table.format")
  }
  if(is.null(format)) {
    format <- "plain"
  }
  
  cat( as.character(x, depth=depth, format, ...) )
}

tabledata <- function(data, slice) {
  if(missing(slice)) {
    slice <- 1:.nrow(data)
  }
  
  self       <- new.env()
  self$data  <- data
  self$slice <- slice
  
  self$format <- function(variables) {
    rows  <- do.call(cbind, lapply( FUN=function(v){ v$format( self ) }, variables ) )
    if(length(rows) > 0) {
      apply(FUN=subtable.row,rows,1)
    } else {
      NULL
    }
  }
  
  self$split <- function(group) {
    nrv <- list()
    for(level in names(group$values)) {
      slice <- which(self[,group$varname] == group$values[level])
      nrv[[level]] <- tabledata(self, slice=slice)
    }
    rowset( nrv )
  }
  
  class(self) <- "gotable.tabledata"
  self
}

#' @export
.nrow.gotable.tabledata <- function(d) {
  length(f$slice)
}

#' @export
print.gotable.tabledata <- function(x,...) {
  print( x[,] )
}

#' @export
`[.gotable.tabledata` <- function(x, i, j) {
  x$data[ x$slice[i], j]
}


.gotable <- function(columns, variables, rows) {
  self           <- new.env()
  
  self$columns   <- columns
  self$rows      <- rows
  
  self$structure <- function() {
    lapply(FUN=function(c) rep(c$align, c$span), self$columns)
  }
  
  self$header <- function() {
    lapply(FUN=function(c) list(c$gettitle(),c$span), self$columns)
  }
  
  self$addcolumn <- function(column) {
    self$columns[[length(self$columns) + 1]]  <- column
  }
  
  self$format <- function() {
    self$rows$format(self$columns)
  }
  
  self$split <- function(group) {
    self$rows <- self$rows$split( group )
  }
  
  class(self)    <- "gotable"
  self
}

#' @export
gotable <- function(data) {
  .gotable( columns   = list(), 
            variables = list(),
            rows      = tabledata( data ) )
}

gotable.clone <- function(table) {
  .gotable( columns   = table$columns, 
            variables = table$variables, 
            rows      = table$rows )
}

format.latexheader <- function(hdr) {
  lapply( FUN=function(h) "\\multicolumn{" %:% h[2] %:% "}{c}{" %:% h[1] %:% "}", hdr )
}

#' @export
as.character.gotable <- function(x, format, ...) {
  if(missing(format) || is.null(format)) {
    format = getOption("knitr.table.format")
  }
  if(is.null(format)) {
    format <- "plain"
  }
  nrv <- ""
  
  content <- x$format()
  if(!is.null(content)) {
    sp <- lapply(FUN=span, content) %>% unlist() %>% max()
  } else {
    sp <- 0
  }
  
  if(format == "latex") {
    delim <- format(" & ", width=7, justify="right" )
    
    ## Start environment
    nrv       <- "\\begin{tabular}{" %:% paste(rep("l",sp),collapse="")  %:% paste(x$structure(), collapse="" ) %:% "}\n"
    
    ## Header structure
    nrv       <- nrv %:% "\\toprule\n"
    if( sp > 0) {
      nrv <- nrv %:% paste( rep(delim, sp), collapse="")
    }
    nrv       <- nrv %:% paste( format.latexheader( x$header()), collapse=" & ") %:% "\\\\\n"
    nrv       <- nrv %:% "\\midrule\n"
  }

  nrv <- nrv %:% paste((lapply(FUN=function(c) as.character(c, format=format), content)), collapse="")
  if(format == "latex") {
    ## End environment
    nrv       <- nrv %:% "\\midrule[\\heavyrulewidth]\n"
    nrv       <- nrv %:% "\\end{tabular}\n"
    nrv
  }
  
  nrv 
}

#' @export
print.gotable <- function(x, format, ...) {
  if(missing(format) || is.null(format)) {
    format = getOption("knitr.table.format")
  }
  if(is.null(format)) {
    format <- "plain"
  }
  cat( as.character(x, format, ...) )
}

#' @export
knit_print.gotable <- function(x, ...) {
  knitr::asis_output(as.character(x, format="latex" ) )
}

addtable <- function(def, table) {
  UseMethod("addtable")
}

#' @export
"+.gotable" <- function(e1, e2) {
  # We just reverse here, to get the correct callback on the second part
  addtable(e2, e1)
}