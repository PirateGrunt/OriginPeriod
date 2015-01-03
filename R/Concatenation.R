#' @include Definition.R
#' 
#' @name Concatenation
#' @title Concatenation
#' 
#' @export 
#' @rdname Concatenation
#' @return OriginPeriod object
#' 
#' @param deparse.level I don't know what this is
#' @param ... OriginPeriod objects to bind
rbind.OriginPeriod = function(..., deparse.level=1){
  elements = list(...)
  blnOPs = sapply(elements, is.OriginPeriod)
  elements = elements[blnOPs]
  
  startDates = lapply(elements, slot, "StartDate")
  startDates = do.call(c, startDates)
  
  monikers = c(unlist(lapply(elements, slot, "Moniker")))
  
  # Period and Type ought to be the same
  Period = elements[[1]]@Period
  type = elements[[1]]@Type
  
  x = OriginPeriod(StartDate = startDates, Period=Period, Moniker=monikers, Type=type)
  x
}

#' @export 
#' 
#' @return OriginPeriod object
#' @rdname Concatenation
#' 
#' @param x OriginPeriod object
#' @param recursive Not used
setMethod("c", signature(x="OriginPeriod"), function(x, ...){
  elements = list(...)
  blnOPs = sapply(elements, is.OriginPeriod)
  elements = elements[blnOPs]
  
  startDates = lapply(elements, slot, "StartDate")
  startDates = do.call("c", startDates)
  startDates = c(x@StartDate, startDates)
  monikers = c(unlist(lapply(elements, slot, "Moniker")))
  monikers = c(x@Moniker, monikers)
  
  # Period and Type ought to be the same
  Period = x@Period
  type = x@Type
  
  x = OriginPeriod(StartDate = startDates, Period=Period, Moniker=monikers, Type=type)
  x
})

#' @name Grow
#' @title Grow
#' 
#' @description 
#' Grow will expand the length of an OriginPeriod object by an arbitrary length.
#' 
#' @export
#' 
#' @param object OriginPeriod object
#' @param ... Additional arguments
setGeneric("Grow", def=function(object, ...){
  standardGeneric("Grow")
})

#' @rdname Grow
#' @param Length Number of elements to add
setMethod("Grow", signature=c(object="OriginPeriod"), definition=function(object, Length){
  startDates = max(object@StartDate)  + object@Period * (1:Length)
  moniker = paste("New moniker", (1:Length))
  op = OriginPeriod(StartDate = startDates, Type=object@Type, Period=object@Period, Moniker=moniker)
  op = c(object, op)
  op
})
