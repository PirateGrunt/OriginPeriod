#' @include Definition.R
#'
#' @title Comparison
#' @param e1 OriginPeriod vector
#' @param e2 OriginPeriod vector
#' @name Comparison
NULL

#' @export
#' @rdname Comparison
setMethod("==", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  sameLength = length(e1) == length(e2)
  sameStart = e1@StartDate[1] == e2@StartDate[1]
  samePeriod = e1@Period == e2@Period
  
  if (length(e1@Type) == 0 & length(e2@Type) == 0){
    sameType = TRUE
  } else if (length(e1@Type) == 0 | length(e2@Type) == 0) {
    sameType = FALSE
  } else {
    sameType = e1@Type == e2@Type  
  }
  
  same = sameLength & sameStart & samePeriod & sameType
  same
})

#' @export
#' @rdname Comparison
setMethod("!=", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  !(e1 == e2)
})

#' @export
#' @rdname Comparison
setMethod(">", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning("> is not defined for an object of class OriginPeriod")
})

#' @export
#' @rdname Comparison
setMethod(">=", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning(">= is not defined for an object of class OriginPeriod")
})

#' @export
#' @rdname Comparison
setMethod("<", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning("< is not defined for an object of class OriginPeriod")
})

#' @export
#' @rdname Comparison
setMethod("<=", signature(e1 = "OriginPeriod", e2 = "OriginPeriod"), function(e1, e2) {
  warning("<= is not defined for an object of class OriginPeriod")
})
