#**********************************************************************************************
# 3. Properties ====

#' @export 
setMethod("show", signature(object="OriginPeriod"), definition=function(object){
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
})

#' @export
setMethod("length", signature=c(x="OriginPeriod"), definition=function(x){
  length(x@StartDate)
})
