#' @include Definition.R
#' 
#' @name Accessors
#' @title Accessors
#' 
#' @export 
#' @rdname Accessors-methods
#' 
#' @aliases [,OriginPeriod,ANY-method
#' 
#' @param x OriginPeriod object
#' @param i Which OriginPeriod element
#' @param j Not used
#' @param ... Not used
#' 
#`[.OriginPeriod` <- function(x, i){
setMethod("[", signature(x="OriginPeriod", i="ANY"), definition=function(x, i){
  op = OriginPeriod(x@StartDate[i], x@EndDate[i]
                    , Period = x@Period
                    , Moniker=x@Moniker[i]
                    , Type=x@Type)
})

#' @export 
#' @rdname Accessors-methods
#' 
#' @aliases [,OriginPeriod,character-method
#' 
setMethod("[", signature(x="OriginPeriod", i="character"), definition=function(x, i){
  i = match(i, x@Moniker)
  op = OriginPeriod(x@StartDate[i], x@EndDate[i], Period = x@Period, Moniker=x@Moniker[i], Type=x@Type)
})

setGeneric("[<-")
#' @export
#' @rdname Accessors-methods
#' 
#' @param value OriginPeriod object
setMethod("[<-", signature(x = "OriginPeriod", i="ANY", j="ANY", value = "OriginPeriod")
          , definition=function(x, i, j, ..., value) {
  if (x@Period != value@Period) {
    msg = "Period length is not equal to object being assigned."
    msg = paste(msg, x@Period, "vs.", value@Period)
    stop(msg)
  }
  x@StartDate[i] = value@StartDate
  x@EndDate[i] = value@EndDate
  x@Moniker[i] = as.character(value@Moniker)
  x
})

#' @export
#' @rdname Accessors-methods
#' 
#' @param name Slot to alter
setMethod("$", signature(x = "OriginPeriod"), function(x, name) {
  slot(x, name)
})

#' @export
#' @rdname Accessors-methods
setMethod("$<-", signature(x = "OriginPeriod"), function(x, name, value) {
  slot(x, name) <- value
  if (!validObject(x)){
    stop("Assignment of this property would create an invalid object. Property not assigned.")
  }
  x
})
