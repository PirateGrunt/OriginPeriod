#**********************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("OriginPeriod"), function(x, ...){
  type = ifelse(length(x@Type)==0, "No type specified", x@Type)
  y = data.frame(StartDate = x@StartDate
                 , EndDate = x@EndDate
                 , Moniker = x@Moniker
                 , Type = rep(type, length(x))
                 , Period = rep(x@Period, length(x)))
  y
})
