#**********************************************************************************************
# 1. Class Definition ====
checkOriginPeriod = function(object)
{
  errors = character()
  
  if (length(object@StartDate) != length(unique(object@StartDate))) {
    errors = c(errors, "Start dates must be unique.")
  }
  
  if (length(object@EndDate) != length(unique(object@EndDate))) {
    errors = c(errors, "End dates must be unique.")
  }
  
  if (length(object@StartDate) != length(object@EndDate)) {
    errors = c(errors, "Start and end date must have the same length.")
  }
  
  if (any(diff(object@StartDate) <= 0)) {
    errors = c(errors, "Start dates must be strictly increasing.")
  }
  
  if (any(object@EndDate <= object@StartDate)) {
    errors = c(errors, "End dates must be strictly greater than start dates.")
  }
  
  if (length(object@Moniker) != length(unique(object@Moniker))) {
    errors = c(errors, "Monikers must be unique.")
  }
  
  if (length(object@Moniker) != length(object@StartDate)){
    errors = c(errors, "Moniker and StartDate don't have equal length.")
  }
  
  if (length(errors) == 0) TRUE else errors
}

setClass("OriginPeriod"
         , representation(StartDate = "Date"
                          , EndDate = "Date"
                          , Moniker = "character"
                          , Period = "Period"
                          , Type = "character")
         , validity = checkOriginPeriod
)

#' is.OriginPeriod
#' 
#' Checks whether the object is an object of the OriginPeriod class.
#' 
#' @param object Object to be tested
#' 
#' @return TRUE/FALSE
#' 
#' @usage is.OriginPeriod(object)
#' @export
#' 
is.OriginPeriod = function(object)
{
  is(object, "OriginPeriod")
}
