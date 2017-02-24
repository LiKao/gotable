# Some usefull helper functions

`%>%` <- dplyr::`%>%`
`%_%` <- function(e1,e2) { paste(  e1, e2 ) }
`%:%` <- function(e1,e2) { paste0( e1, e2 ) }
`%/%` <- function(e1,e2) { paste0( e1, e2, "\n")}
getl <- function(l, n) { lapply( l, `[[`, n ) }