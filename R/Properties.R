#' @include Definition.R
#' 
#' @name show
#' @title show
#' 
#' @export 
#' @param object OriginPeriod object
#' 
show.OriginPeriod <- function(object){
  cat("OriginPeriod object \n")
  if (length(object@Type) == 0) {
    cat("Type:\tNo type specified\n")
  } else {
    cat("Type:\t", object@Type, "\n")  
  }
  cat("Period:\t", as.character(object@Period), "\n")
  cat("First date:\t", as.character(min(object@StartDate)), "\n")
  cat("Last date:\t", as.character(max(object@EndDate)), "\n")
  cat("Length:\t", length(object), "\n")
}

#' @name length
#' @title length
#' 
#' @param x OriginPeriod object
#' @export
length.OriginPeriod <- function(x){
  length(x@StartDate)
}
