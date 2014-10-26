#**********************************************************************************************
# 7. Concatenation ====

#' @export 
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

#' @export
setMethod("Grow", signature=c(object="OriginPeriod"), definition=function(object, Length){
  startDates = max(object@StartDate)  + object@Period * (1:Length)
  moniker = paste("New moniker", (1:Length))
  op = OriginPeriod(StartDate = startDates, Type=object@Type, Period=object@Period, Moniker=moniker)
  op = c(object, op)
  op
})
