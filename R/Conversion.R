#' @include Definition.R
#' 
#' @name Conversion
#' @title Conversion
#' 
#' @description
#' Converts an OriginPeriod object to a data frame
#' 
#' @param x OriginPeriod object
#' @param ... Additional arguments
#' 
#' @export
as.data.frame.OriginPeriod <- function(x, ...){
  type = ifelse(length(x@Type)==0, "No type specified", x@Type)
  y = data.frame(StartDate = x@StartDate
                 , EndDate = x@EndDate
                 , Moniker = x@Moniker
                 , Type = rep(type, length(x))
                 , Period = rep(x@Period, length(x)))
  y
}
