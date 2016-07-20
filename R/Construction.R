#' @include Definition.R
#' @include Support.R
#' 
#' @title OriginPeriod
#' 
#' @description
#' Construct an OriginPeriod object 
#' 
#' @param StartDate vector of start dates
#' @param EndDate vector of end dates
#' @param Period The period between the start and end dates
#' @param ... Additional parameters
#' 
#' @details
#' An OriginPeriod object may be constructed in a number of ways.
#' 
#' 1. Provide start and end dates
#' 
setGeneric("OriginPeriod", function(StartDate, EndDate, Period, ...) {
  standardGeneric("OriginPeriod")
})

#' @export
#' 
#' @rdname OriginPeriod
#' 
#' @param Moniker Character vector of names for each element
#' @param Type "Accident year", "Report year", etc.
#' @param Verbose Display warnings?
#' @param NumPeriods How many periods?
#' @param StartMonth Starting month of the period
#' @param StartDay Starting day of the period
#' 
setMethod("OriginPeriod", signature=c(StartDate="ANY", EndDate="ANY", Period="ANY")
          , definition = function(StartDate, EndDate, Period, Moniker, Type, Verbose=FALSE, NumPeriods, StartMonth, StartDay){
            
            if (missing(StartDate)){
              stop("StartDate must be specified")
            }
            
            isPOSIX = intersect(class(StartDate) , c("POSIXct", "POSIXt"))
            if (length(isPOSIX) != 0) {
              StartDate = as.Date(StartDate)
              if (Verbose) message("StartDate was given as a POSIX date/time value. It has been converted to class 'Date'.")
            }
            
            if (class(StartDate) == "numeric"){
              StartDate = as.integer(StartDate)
              if (Verbose) message("StartDate has been converted to an integer. Check that your results are as you expect.")
            }
            
            if (class(StartDate) == "integer") {
              if(missing(StartMonth)) {StartMonth = 1}
              if(missing(StartDay)) {StartDay = 1}
              
              StartDate = as.Date(paste(StartDate, StartMonth, StartDay, sep="-"))
            }
            
            if (missing(EndDate)){
              if (length(StartDate) != 1){
                NumPeriods = length(StartDate)
              } else {
                if (missing(NumPeriods)) NumPeriods = 1
              }
              if(missing(Period)) Period = DefaultPeriod()
            } else {
              if (length(StartDate) != length(EndDate)) {
                errMsg = "StartDate and EndDate must be of equal length."
                errMsg = paste0(errMsg, "StartDate has length ", length(StartDate), ".")
                errMsg = paste0(errMsg, "EndDate has length ", length(EndDate), ".")
                stop(errMsg)
              }
              
              if (length(StartDate) == 1){
                if (missing(Period)) Period = DefaultPeriod()
                NumPeriods = PeriodsBetween(StartDate, EndDate, Period)
              } else {
                if (missing(Period)) Period = InferPeriod(StartDate, EndDate)
                NumPeriods = length(StartDate)
              }
            }
            
            StartDate = StartDate[1] + Period * (0:(NumPeriods-1))
            EndDate = StartDate + Period - days(1)
            
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            
            if (missing(Type)) {Type = character()}
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
            op
})
